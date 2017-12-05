(ns aoc17.d3
  (:require [clojure.string :as s]))

(defn q1 [n]
  (let [sq #(+ (- (* 4 % %) (* 4 %)) 2)
        i (loop [i 1]
            (if (>= n (sq i))
              (recur (inc i))
              (dec i)))]
    (loop [s (sq i)
           [ds f] [(dec (* 2 i)) dec]]
      (if (= n s) ds
        (recur (inc s)
               (condp = ds
                 i [(inc ds) inc]
                 (* 2 i) [(dec ds) dec]
                 [(f ds) f]))))))

(defn q2 [x]
  (loop [m (transient {[0 0] 1})
         [r c d] [0 1 :r]]
    (let [v (apply + (for [x [(dec c) c (inc c)]
                           y [(dec r) r (inc r)]]
                       (get m [y x] 0)))]
      (if (>= v x) v
        (recur (assoc! m [r c] v)
               (let [d (case d
                         :r (if (get m [(dec r) c]) d :u)
                         :u (if (get m [r (dec c)]) d :l)
                         :l (if (get m [(inc r) c]) d :d)
                         :d (if (get m [r (inc c)]) d :r))]
                 (case d
                   :u  [(dec r) c d]
                   :l  [r (dec c) d]
                   :d  [(inc r) c d]
                   :r  [r (inc c) d])))))))

(println "q1" (q1 312051))
(println "q2" (q2 312051))
