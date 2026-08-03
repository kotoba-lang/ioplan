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
