(ns ioplan.core
  "Turning a list of I/O requests into the list the device actually wants.

  Four transformations, and which of them apply is a property of the device
  rather than a preference:

  1. **align** every request to whole device blocks, because a device cannot
     transfer half a block and pretending otherwise hides the amplification;
  2. **merge** adjacent requests, up to the device's maximum transfer, so one
     command carries what several would have;
  3. **order** them — or, on a device with no seek cost, deliberately do not;
  4. **batch** them to the device's queue depth.

  The third is where most of the wrong intuition lives. An elevator sort is
  the canonical I/O optimization and on an NVMe device it is *harmful*: it
  spends CPU and latency to reduce a cost that is zero, and it destroys the
  submission order the caller chose. `machine.core/reorderable?` answers the
  question from the device descriptor, so a plan cannot apply a rotating-disk
  optimization to flash by inheritance.

  Everything is a pure function of requests plus a device descriptor. No I/O
  happens here; the output is a plan a host executes.

  Pure `.cljc`. Depends only on `kotoba-lang/machine`."
  (:require [machine.core :as m]))

(def format-id :kotoba.ioplan/v1)

;; ── requests ─────────────────────────────────────────────────────────────

(defn request-errors [requests]
  (vec
   (concat
    (when-not (vector? requests) [{:error :requests-not-a-vector}])
    (when (vector? requests)
      (concat
       (for [[i r] (map-indexed vector requests)
             e (cond-> []
                 (not (#{:read :write} (:op r)))     (conj :invalid-op)
                 (not (and (integer? (:offset r)) (<= 0 (:offset r)))) (conj :invalid-offset)
                 (not (pos-int? (:bytes r)))         (conj :invalid-bytes))]
         {:error e :index i :request r})
       (let [ids (keep :id requests)]
         (when-not (= (count ids) (count (set ids)))
           [{:error :duplicate-request-id :ids (vec ids)}])))))))

(defn- check! [requests]
  (let [errs (request-errors requests)]
    (when (seq errs)
      (throw (ex-info "invalid request list" {:phase :ioplan/requests :errors errs})))
    requests))

(defn- device! [machine id]
  (or (m/storage-device machine id)
      (throw (ex-info "machine declares no such storage device"
                      {:phase :ioplan/device :device id
                       :known (mapv :id (:storage machine))}))))

;; ── align ────────────────────────────────────────────────────────────────

(defn align
  "Expand each request to whole device blocks.

  The expansion is not overhead being added, it is overhead being *made
  visible*: the device was always going to move whole blocks. A 100-byte read
  from a 4 KiB-block device transfers 4096 bytes, and a plan that reports 100
  is lying about the bandwidth it will consume."
  [device requests]
  (let [b (:block-bytes device)]
    (mapv (fn [r]
            (let [start (* b (quot (:offset r) b))
                  end (* b (quot (+ (:offset r) (:bytes r) (dec b)) b))]
              (assoc r
                     :aligned-offset start
                     :aligned-bytes (- end start)
                     :covers [(:id r)])))
          requests)))

;; ── merge ────────────────────────────────────────────────────────────────

(defn default-merge-gap
  "How far apart two requests may be and still be worth merging.

  On a seeking device, skipping a gap costs a seek and reading through it
  costs only bandwidth, so merging across a block-sized hole wins. On a device
  with no seek cost there is nothing to buy, so the gap is zero and only
  genuinely adjacent requests combine."
  [device]
  (if (m/reorderable? device) (:block-bytes device) 0))

(defn merge-adjacent
  "Combine requests that touch, or nearly touch, into single transfers.

  Requests must already be aligned. Merging never crosses an op boundary — a
  read absorbed into a write would return bytes that were never read — and
  never exceeds the device's maximum transfer, because a command the device
  cannot express is not a plan."
  ([device requests] (merge-adjacent device requests (default-merge-gap device)))
  ([device requests gap]
   (let [maxb (:max-transfer-bytes device)
         ;; On a seeking device the whole list may be sorted first, so any two
         ;; requests that touch will find each other. On a zero-seek device
         ;; that sort would be a reordering by the back door — merging must
         ;; not undo the ordering decision `order` is about to decline to
         ;; make — so only neighbours in SUBMISSION order combine, which
         ;; still catches the sequential reader that produced them.
         sorted (if (m/reorderable? device)
                  (vec (sort-by (juxt :aligned-offset (comp name :op)) requests))
                  (vec requests))]
     (reduce
      (fn [acc r]
        (let [prev (peek acc)
              end (when prev (+ (:aligned-offset prev) (:aligned-bytes prev)))
              new-end (+ (:aligned-offset r) (:aligned-bytes r))
              mergeable? (and prev
                              (= (:op prev) (:op r))
                              ;; Both bounds. Only checking the upper one
                              ;; merges a request that lies BEFORE prev --
                              ;; harmless on a sorted list and silently wrong
                              ;; on the unsorted one a zero-seek device gets.
                              (>= (:aligned-offset r) (:aligned-offset prev))
                              (<= (:aligned-offset r) (+ end gap))
                              (<= (- (max end new-end) (:aligned-offset prev)) maxb))]
          (if mergeable?
            (conj (pop acc)
                  (assoc prev
                         :aligned-bytes (- (max end new-end) (:aligned-offset prev))
                         :covers (into (:covers prev) (:covers r))
                         :merged? true))
            (conj acc r))))
      []
      sorted))))

;; ── order ────────────────────────────────────────────────────────────────

(defn head-travel
  "Total head movement visiting these requests in this order from `start`."
  [requests start]
  (first
   (reduce (fn [[total at] r]
             [(+ total (Math/abs (- (:aligned-offset r) at))) (:aligned-offset r)])
           [0 start]
           requests)))

(defn order
  "Sequence the requests the way this device is actually paid to receive them.

  `:high` — C-SCAN: one ascending sweep. Ascending-only rather than
  back-and-forth because a bidirectional sweep starves whichever end the head
  is walking away from, and the starved request is usually the interactive
  one.

  `:low` — sorted, but no sweep discipline. There is a cost to locality but
  no head to walk.

  `:none` — **submission order, untouched**. Reordering buys nothing here and
  costs the ordering the caller chose. This is the branch that stops an
  elevator sort from being applied to flash out of habit."
  [device requests]
  (case (:seek-cost device)
    :none requests
    :low (vec (sort-by (juxt (comp name :op) :aligned-offset) requests))
    :high (vec (sort-by :aligned-offset requests))))

;; ── batch ────────────────────────────────────────────────────────────────

(defn batch
  "Split into waves the device can have in flight at once.

  On a zero-seek device this is the *only* thing that matters — throughput
  comes from keeping the queue full, not from the order within it."
  [device requests]
  (mapv vec (partition-all (:queue-depth device) requests)))

;; ── the plan ─────────────────────────────────────────────────────────────

(def plan-model
  {:model/id :kotoba.ioplan/block-transfers-v1
   :model/counts [:bytes-requested :bytes-transferred :read-amplification
                  :head-travel :commands]
   :model/assumes
   ["a request costs one block-aligned transfer plus, on a seeking device, the
     head movement to reach it"
    "merging is free once the bytes are already being moved"
    "queue depth bounds concurrency, not bandwidth"]
   :model/does-not-model
   [:controller-queueing :garbage-collection :thermal-throttling :cache-on-device
    :rotational-latency :parallelism-across-channels]})

(defn plan
  "Align, merge, order and batch, reporting what each step bought.

  `:read-amplification` is bytes the device will move divided by bytes the
  caller asked for. Below 1.0 means merging removed duplicate coverage;
  above means block alignment is dominating, which on a 4 KiB device with
  100-byte reads is a factor of 40 and worth seeing before optimising
  anything else."
  [machine device-id requests & [{:keys [head merge-gap] :or {head 0}}]]
  (check! requests)
  (let [device (device! machine device-id)
        aligned (align device requests)
        merged (merge-adjacent device aligned (or merge-gap (default-merge-gap device)))
        ordered (order device merged)
        batches (batch device ordered)
        requested (reduce + 0 (map :bytes requests))
        transferred (reduce + 0 (map :aligned-bytes merged))
        writes (filterv #(= :write (:op %)) requests)
        write-requested (reduce + 0 (map :bytes writes))
        write-transferred (reduce + 0 (map :aligned-bytes
                                           (filter #(= :write (:op %)) merged)))]
    {:format format-id
     :device device
     :batches batches
     :order (mapv :aligned-offset ordered)
     :stats
     (cond-> {:requests-in (count requests)
              :commands (count merged)
              :merges (- (count aligned) (count merged))
              :batches (count batches)
              :bytes-requested requested
              :bytes-transferred transferred
              :read-amplification (if (pos? requested)
                                    (double (/ transferred requested))
                                    0.0)
              ;; Did the pipeline emit commands out of submission order?
              ;; Comparing offset lists cannot answer this once merging has
              ;; changed the command count, so rank each command by where its
              ;; first covered request was submitted.
              :reordered? (let [rank (into {} (map-indexed (fn [i r] [(:id r) i]) requests))
                                ranks (mapv #(get rank (first (:covers %))) ordered)]
                            (not= ranks (vec (sort ranks))))
              :head-travel (when (m/reorderable? device) (head-travel ordered head))
              ;; The baseline is submission order — what the device would have
              ;; been handed with no planner at all. Comparing against the
              ;; post-merge list would flatter the elevator, since merging has
              ;; already sorted.
              :head-travel-submitted (when (m/reorderable? device)
                                       (head-travel aligned head))}
       (seq writes)
       (assoc :write-amplification
              ;; Bytes the device must actually write for the bytes the caller
              ;; wanted written. A sub-block write is a read-modify-write, and
              ;; on flash that costs an erase block somewhere later.
              (if (pos? write-requested)
                (double (/ write-transferred write-requested))
                0.0)))
     :model plan-model
     :machine (:machine/id machine)}))

(defn explain
  "One line per decision the plan made, for a human reading a slow trace."
  [p]
  (let [{:keys [device stats]} p]
    [(str "device " (:id device) " (" (name (:kind device))
          ", block " (:block-bytes device) "B, queue " (:queue-depth device)
          ", seek-cost " (name (:seek-cost device)) ")")
     (str (:requests-in stats) " requests -> " (:commands stats) " commands ("
          (:merges stats) " merged) in " (:batches stats) " batches")
     (str "moved " (:bytes-transferred stats) "B for " (:bytes-requested stats)
          "B asked (amplification " (:read-amplification stats) ")")
     (if (m/reorderable? device)
       (str "reordered: head travel " (:head-travel stats) "B against "
            (:head-travel-submitted stats) "B as submitted")
       "not reordered: this device charges nothing for seeking, so submission order stands")]))
