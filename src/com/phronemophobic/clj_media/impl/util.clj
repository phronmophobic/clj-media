(ns com.phronemophobic.clj-media.impl.util
  (:require [clojure.data.priority-map :refer [priority-map]]))

(defn distinct-by
  "Returns a lazy sequence of the elements of coll with duplicates removed.
  Returns a stateful transducer when no collection is provided."
  ([keyfn]
   (fn [rf]
     (let [seen (volatile! #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [k (keyfn input)]
            (if (contains? @seen k)
              result
              (do (vswap! seen conj k)
                  (rf result input))))))))))

(defn ^:private interleave-all-by* [f coll-map colls]
  (lazy-seq
   (loop [kvs (seq coll-map)
          coll-map coll-map]
     (when kvs
       (let [kv (first kvs)
             k (key kv)
             xs (seq (nth colls k))]
         (if xs
           (let [x (first xs)]
             (cons x
                   (let [coll-map (assoc coll-map k (f x))
                         xs-rest (rest xs)
                         colls (assoc colls k xs-rest)]
                     (interleave-all-by* f coll-map colls))))
           ;; else
           (recur (next kvs)
                  (dissoc coll-map k))))))))

(defn interleave-all-by [f & colls]
  (let [colls (into []
                    (keep seq)
                    colls)]
    (interleave-all-by* f
                        (into (priority-map)
                              (map-indexed (fn [i xs]
                                             [i (f (first xs))]))
                              colls)
                        colls)))


(defn queue
  ([]
   clojure.lang.PersistentQueue/EMPTY)
  ([coll]
   (into (queue) coll)))


(defn ^:private interleave-all* [colls]
  (lazy-seq
   (loop [colls colls]
     (let [coll (peek colls)]
       (when coll
         (let [s (seq coll)]
           (if s
             (cons (first s)
                   (interleave-all* (-> colls
                                        pop
                                        (conj (rest s)))))
             (recur (pop colls)))))))))

(defn interleave-all [& colls]
  (interleave-all* (queue colls)))

;; todo, move to util
(defn insert-last [x]
  (fn [rf]
    (fn
      ([] (rf))
      ([result]
       (let [result (rf result x)]
         (rf (unreduced result))))
      ([result input] (rf result input)))))

(comment
  (interleave-all-by :i
                     (map (fn [i] {:i i}) (range 2 10))
                     (map (fn [i] {:i i}) (range 6))
                     )

  ,)




