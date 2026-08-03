(ns ioplan.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [ioplan.core :as io]
            [machine.core :as m]))

(def mach
  {:format m/format-id
   :machine/id "fixture"
   :machine/provenance :measured
   :machine/source "test fixture"
   :storage [{:id :nvme :kind :nvme :block-bytes 4096 :queue-depth 4
              :seek-cost :none :max-transfer-bytes 131072}
             {:id :disk :kind :hdd :block-bytes 4096 :queue-depth 1
              :seek-cost :high :max-transfer-bytes 1048576}
             {:id :ssd :kind :sata-ssd :block-bytes 4096 :queue-depth 32
              :seek-cost :low :max-transfer-bytes 262144}]})

(defn- r [id off bytes & [op]]
  {:id id :op (or op :read) :offset off :bytes bytes})

(deftest fixture-machine-is-valid (is (m/valid? mach)))

(deftest an-unknown-device-is-refused
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
               (io/plan mach :ssd-that-does-not-exist [(r :a 0 4096)]))))

(deftest bad-requests-are-refused
  (is (some #(= :invalid-op (:error %)) (io/request-errors [(r :a 0 4096 :trim)])))
  (is (some #(= :invalid-offset (:error %)) (io/request-errors [(r :a -1 4096)])))
  (is (some #(= :invalid-bytes (:error %)) (io/request-errors [(r :a 0 0)])))
  (is (some #(= :duplicate-request-id (:error %))
            (io/request-errors [(r :a 0 4096) (r :a 8192 4096)]))))

;; ── alignment ────────────────────────────────────────────────────────────

(deftest alignment-makes-amplification-visible-rather-than-adding-it
  (let [p (io/plan mach :nvme [(r :a 100 100)])]
    (testing "the device was always going to move a whole block"
      (is (= 100 (get-in p [:stats :bytes-requested])))
      (is (= 4096 (get-in p [:stats :bytes-transferred])))
      (is (= 40.96 (get-in p [:stats :read-amplification]))))))

(deftest a-request-straddling-a-block-boundary-costs-two
  (let [p (io/plan mach :nvme [(r :a 4090 12)])]
    (is (= 8192 (get-in p [:stats :bytes-transferred])))
    (is (= 0 (:aligned-offset (ffirst (:batches p)))))))

;; ── merging ──────────────────────────────────────────────────────────────

(deftest adjacent-requests-become-one-command
  (let [p (io/plan mach :nvme [(r :a 0 4096) (r :b 4096 4096) (r :c 8192 4096)])]
    (is (= 1 (get-in p [:stats :commands])))
    (is (= 2 (get-in p [:stats :merges])))
    (is (= [:a :b :c] (:covers (ffirst (:batches p)))))
    (testing "and the transfer is exactly what was asked for"
      (is (= 1.0 (get-in p [:stats :read-amplification]))))))

(deftest merging-never-crosses-an-op-boundary
  (testing "a read absorbed into a write would return bytes nobody read"
    (let [p (io/plan mach :nvme [(r :a 0 4096 :read) (r :b 4096 4096 :write)])]
      (is (= 2 (get-in p [:stats :commands]))))))

(deftest merging-stops-at-the-devices-maximum-transfer
  (let [reqs (mapv (fn [i] (r (keyword (str "r" i)) (* i 4096) 4096)) (range 64))
        p (io/plan mach :nvme reqs)]
    (testing "64 blocks is 256 KiB against a 128 KiB maximum, so two commands"
      (is (= 2 (get-in p [:stats :commands])))
      (is (every? #(<= (:aligned-bytes %) 131072) (apply concat (:batches p)))))))

(deftest a-seeking-device-merges-across-a-small-hole
  (testing "skipping the gap costs a seek; reading through it costs bandwidth"
    (let [p (io/plan mach :disk [(r :a 0 4096) (r :b 8192 4096)])]
      (is (= 1 (get-in p [:stats :commands])))
      (is (= 12288 (get-in p [:stats :bytes-transferred])))))
  (testing "on a zero-seek device there is nothing to buy, so it does not"
    (let [p (io/plan mach :nvme [(r :a 0 4096) (r :b 8192 4096)])]
      (is (= 2 (get-in p [:stats :commands]))))))

;; ── ordering ─────────────────────────────────────────────────────────────

(def ^:private scattered
  [(r :a 40960 4096) (r :b 4096 4096) (r :c 81920 4096)
   (r :d 0 4096) (r :e 61440 4096)])

(deftest an-elevator-sort-helps-a-seeking-device
  (let [p (io/plan mach :disk scattered)
        {:keys [head-travel head-travel-submitted]} (:stats p)]
    (is (:reordered? (:stats p)))
    (testing "0 and 4096 touch, so they arrive as one command"
      (is (= [0 40960 61440 81920] (:order p))))
    (testing "one ascending sweep: travel is the span, not the zig-zag"
      (is (= 81920 head-travel))
      (is (< head-travel head-travel-submitted)))))

(deftest the-same-sort-is-refused-on-a-device-that-charges-nothing-to-seek
  (let [p (io/plan mach :nvme scattered)]
    (testing "reordering buys nothing and costs the order the caller chose"
      (is (not (:reordered? (:stats p))))
      (is (= [40960 4096 81920 0 61440] (:order p)))
      (is (nil? (get-in p [:stats :head-travel]))))))

(deftest merging-does-not-reorder-a-zero-seek-device-by-the-back-door
  (testing "only submission neighbours combine there, so the plan stays in order"
    (let [p (io/plan mach :nvme [(r :a 40960 4096) (r :b 0 4096) (r :c 4096 4096)])]
      (is (= [40960 0] (:order p)))
      (is (= 2 (get-in p [:stats :commands])))
      (is (not (:reordered? (:stats p))))))
  (testing "while a seeking device is free to sort first and merge more"
    (let [p (io/plan mach :disk [(r :a 40960 4096) (r :b 0 4096) (r :c 4096 4096)])]
      (is (= 2 (get-in p [:stats :commands])))
      (is (= [0 40960] (:order p))))))

(deftest head-travel-counts-from-where-the-head-is
  (is (= 81920 (io/head-travel (io/align (m/storage-device mach :disk)
                                         [(r :a 81920 4096)]) 0)))
  (is (= 4096 (io/head-travel (io/align (m/storage-device mach :disk)
                                        [(r :a 81920 4096)]) 77824))))

;; ── batching ─────────────────────────────────────────────────────────────

(deftest batches-follow-the-devices-queue-depth
  (let [reqs (mapv (fn [i] (r (keyword (str "r" i)) (* i 65536) 4096)) (range 10))
        p (io/plan mach :nvme reqs)]
    (testing "queue depth 4, nothing mergeable: three batches"
      (is (= 10 (get-in p [:stats :commands])))
      (is (= 3 (get-in p [:stats :batches])))
      (is (= [4 4 2] (mapv count (:batches p))))))
  (testing "a single-queue disk gets one request per batch"
    (let [p (io/plan mach :disk scattered)]
      (is (= 1 (:queue-depth (:device p))))
      (is (every? #(= 1 (count %)) (:batches p))))))

;; ── writes ───────────────────────────────────────────────────────────────

(deftest a-sub-block-write-is-a-read-modify-write-and-says-so
  (let [p (io/plan mach :nvme [(r :a 0 512 :write)])]
    (is (= 8.0 (get-in p [:stats :write-amplification]))))
  (testing "a whole-block write amplifies by nothing"
    (let [p (io/plan mach :nvme [(r :a 0 4096 :write)])]
      (is (= 1.0 (get-in p [:stats :write-amplification])))))
  (testing "and a read-only workload reports no write amplification at all"
    (is (nil? (get-in (io/plan mach :nvme [(r :a 0 4096)]) [:stats :write-amplification])))))

;; ── plan as a whole ──────────────────────────────────────────────────────

(deftest the-plan-carries-its-model
  (let [p (io/plan mach :disk scattered)]
    (is (= :kotoba.ioplan/block-transfers-v1 (get-in p [:model :model/id])))
    (is (seq (get-in p [:model :model/does-not-model])))))

(deftest explain-says-what-it-did-and-what-it-declined-to-do
  (let [lines (io/explain (io/plan mach :nvme scattered))]
    (is (= 4 (count lines)))
    (is (re-find #"not reordered" (last lines))))
  (let [lines (io/explain (io/plan mach :disk scattered))]
    (is (re-find #"reordered: head travel" (last lines)))))

(deftest planning-is-deterministic
  (let [a (io/plan mach :disk scattered)
        b (io/plan mach :disk scattered)]
    (is (= (:order a) (:order b)))
    (is (= (:stats a) (:stats b)))))

;; ── how much is there to win? ────────────────────────────────────────────

(deftest optimal-travel-has-a-closed-form
  (testing "outside the span you walk it once"
    (is (= 100 (io/optimal-travel [50 100 150] 50)))
    (is (= 100 (io/optimal-travel [50 100 150] 150))))
  (testing "inside it you must double back over one side, so take the near one first"
    (is (= 150 (io/optimal-travel [0 100] 50)))
    (is (= 110 (io/optimal-travel [0 100] 10))))
  (is (= 0 (io/optimal-travel [] 0)))
  (is (= 0 (io/optimal-travel [42] 42))))

(deftest an-already-minimal-request-list-is-not-worth-planning
  (testing "three block-aligned, non-overlapping, non-adjacent reads on flash"
    (let [b (io/benefit mach :nvme [(r :a 0 4096) (r :b 65536 4096) (r :c 131072 4096)])]
      (is (= 12288 (get-in b [:floors :bytes]) (get-in b [:submitted :bytes])))
      (is (= 3 (get-in b [:floors :commands]) (get-in b [:submitted :commands])))
      (is (not (:worth-planning? b)))
      (testing "and no credit is claimed for removing an excess that was zero"
        (is (nil? (get-in b [:captured :bytes])))
        (is (nil? (get-in b [:captured :commands])))))))

(deftest contiguous-requests-have-command-excess-and-the-plan-takes-all-of-it
  (let [b (io/benefit mach :nvme [(r :a 0 4096) (r :b 4096 4096) (r :c 8192 4096)])]
    (testing "one union run, so one command is the floor"
      (is (= 1 (get-in b [:floors :commands])))
      (is (= 3 (get-in b [:submitted :commands]))))
    (is (:worth-planning? b))
    (testing "merging captured the whole excess"
      (is (= 1.0 (get-in b [:captured :commands]))))
    (testing "bytes were already minimal — merging removes overlap, not alignment"
      (is (= 12288 (get-in b [:floors :bytes])))
      (is (nil? (get-in b [:captured :bytes]))))))

(deftest overlapping-requests-are-where-bytes-can-be-removed
  (testing "two reads covering the same block twice"
    (let [b (io/benefit mach :nvme [(r :a 0 4096) (r :b 2048 2048)])]
      (is (= 4096 (get-in b [:floors :bytes])))
      (is (= 8192 (get-in b [:submitted :bytes])))
      (is (:worth-planning? b))
      (is (= 1.0 (get-in b [:captured :bytes]))))))

(deftest travel-is-nil-on-a-device-that-charges-nothing-to-seek
  (testing "not zero — the quantity does not apply rather than being minimal"
    (let [b (io/benefit mach :nvme scattered)]
      (is (nil? (get-in b [:floors :travel])))
      (is (nil? (get-in b [:captured :travel]))))))

(deftest the-elevator-captures-all-of-the-travel-excess-on-a-disk
  (let [b (io/benefit mach :disk scattered {:head 0})]
    (testing "the head starts below the span, so one ascending sweep is optimal"
      (is (= 81920 (get-in b [:floors :travel])))
      (is (= 81920 (get-in b [:planned :travel]))))
    (is (< (get-in b [:floors :travel]) (get-in b [:submitted :travel])))
    (is (= 1.0 (get-in b [:captured :travel])))
    (is (:worth-planning? b))))

(deftest a-mid-span-head-still-lets-c-scan-be-optimal
  (testing "doubling back over the lower half is what the optimum does too"
    (let [b (io/benefit mach :disk scattered {:head 40960})]
      (is (= 122880 (get-in b [:floors :travel])))
      (is (= 1.0 (get-in b [:captured :travel]))))))

(deftest c-scan-from-above-the-span-costs-the-span-twice-over
  (testing "the ascending sweep walks DOWN to the lowest request and then back
            up over the same ground, while the shortest walk just descends. So
            C-SCAN pays (head - lo) + span against an optimum of (head - lo):
            the ratio is 1 + span/(head - lo), which is exactly 2 when the head
            sits on the top of the span and decays toward 1 as it moves away.
            This is the price of the anti-starvation choice `order` documents,
            and `benefit` is what turns it from prose into a number."
    (let [span 81920]
      (testing "head exactly on the top of the span: the worst case, exactly 2x"
        (let [b (io/benefit mach :disk scattered {:head 81920})]
          (is (= 81920 (get-in b [:floors :travel])))
          (is (= 163840 (get-in b [:planned :travel])))))
      (testing "further above, the penalty decays as 1 + span/(head - lo)"
        (let [b (io/benefit mach :disk scattered {:head 100000})
              floor (get-in b [:floors :travel])
              planned (get-in b [:planned :travel])]
          (is (= 100000 floor))
          (is (= (+ floor span) planned))
          (is (< 1.81 (/ (double planned) floor) 1.83)))))))
