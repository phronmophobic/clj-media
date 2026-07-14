(ns com.phronemophobic.clj-media.impl.flow.util
  (:require [clojure.core.async.flow :as flow]
            [clojure.core.async :as async]))

(defn queue
  "Create an empty persistent queue or a persistent queue from a sequence."
  ([] clojure.lang.PersistentQueue/EMPTY)
  ([xs] (into (queue) xs)))

(defn peek-fresh-frame [state]
  (-> state ::fresh-frames peek))
(defn pop-fresh-frame [state] 
  (update state ::fresh-frames pop))

(defn wrap-fresh
  "Must set ::flush-frames on state. Also, don't set ::flow/input-filter."
  [{:keys [flush-kw pending-kw fresh-chan-kw buf-size-kw fresh-queue-kw in-port-kw] :as kws}
   {:keys [describe init transition transform]}]
  (let [
        wrap-input-filter (fn [transform]
                            (fn [state in msg]
                              (assert (not (and (pending-kw state)
                                                (seq (fresh-queue-kw state)))))
                              (let [[state outs] (transform state in msg)
                                    state (cond
                                            
                                            (and (pending-kw state)
                                                 (empty? (fresh-queue-kw state)))
                                            (assoc state ::flow/input-filter #{in-port-kw})
                                            
                                            (>= (count (fresh-queue-kw state))
                                                (buf-size-kw state))
                                            (assoc state ::flow/input-filter (fn [cid]
                                                                               (not= cid in-port-kw)))
                                            
                                            :else (dissoc state ::flow/input-filter))]
                                [state outs])))

        flush
        (fn [state outs]
          (let [f (flush-kw state)]
            (loop [state state
                   outs outs]
              (if (pending-kw state)
                (if-let [frame (peek-fresh-frame state)]
                  (let [[state outs] (f state outs frame)]
                    (recur state outs))
                  [state outs])
                ;; else
                [state outs]))))]
    {:describe (fn []
                 (assoc (if describe (describe) {})
                        fresh-chan-kw "Channel to get fresh frames from"
                        buf-size-kw "Number of fresh frames to buffer"))
     :init (fn [{::keys [fresh-frame-chan frame-buf-size] :as state}]
             (assert fresh-frame-chan )
             (let [state (-> (if init (init state) state)
                             (assoc buf-size-kw (or frame-buf-size 5)
                                    fresh-queue-kw (queue))
                             (assoc-in [::flow/in-ports in-port-kw] fresh-frame-chan))]
               (assert (flush-kw state) "Must set ::flush-frames on state.")
               state))
     :transition (or transition (fn [state _] state))
     :transform
     (wrap-input-filter
      (fn [state in msg]
        (cond 
          (= in in-port-kw)
          (let [state (update state fresh-queue-kw conj msg)]
            (flush state {}))
          
          :else
          (let [[state outs] (transform state in msg)]
            (flush state outs)))))}))


(defn wrap-fresh-frames [{:keys [describe init transition transform] :as p}]
  (wrap-fresh {:flush-kw ::flush-frames
               :pending-kw ::pending-frames?
               :fresh-chan-kw ::fresh-frame-chan
               :buf-size-kw ::frame-buf-size
               :in-port-kw ::fresh-frame
               :fresh-queue-kw ::fresh-frames}
              p))

(defn wrap-fresh-packets [{:keys [describe init transition transform] :as p}]
  (wrap-fresh {:flush-kw ::flush-packets
               :pending-kw ::pending-packets?
               :fresh-chan-kw ::fresh-packet-chan
               :buf-size-kw ::packet-buf-size
               :in-port-kw ::fresh-packet
               :fresh-queue-kw ::fresh-packets}
              p))

