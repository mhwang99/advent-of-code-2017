(ns aoc17.d3
  (:require [clojure.string :as s]))

(defn q1 [n]
  (let [sq (map #(+ (- (* 4 % %) (* 4 %)) 2) (range))]
    (loop [i 1]
      (if (< n (nth sq i))
        (let [i (dec i)]
          (loop [s (nth sq i)
                 ds (dec (* 2 i))
                 f dec]
            (cond
              (= n s) ds
              (= ds i) (recur (inc s) (inc ds) inc)
              (= ds (* 2 i)) (recur (inc s) (dec ds) dec)
              :else (recur (inc s) (f ds) f))))
        (recur (inc i))))))

(defn q2 [x]
  (loop [r 0
         c 1
         m {[0 0] 1}
         d :r]
    (let [v (apply +
                   (for [x (range (dec c) (+ c 2))
                         y (range (dec r) (+ r 2))
                         :when (not= [r c] [y x])]
                     (get m [y x] 0)))]
      (if (>= v x)
        v
        (let [m (assoc m [r c] v)
              [r c d] (case d
                        :r (if (nil? (get m [(dec r) c]))
                             [(dec r) c :u]
                             [r (inc c) d])
                        :u (if (nil? (get m [r (dec c)]))
                             [r (dec c) :l]
                             [(dec r) c d])
                        :l (if (nil? (get m [(inc r) c]))
                             [(inc r) c :d]
                             [r (dec c) d])
                        :d (if (nil? (get m [r (inc c)]))
                             [r (inc c) :r]
                             [(inc r) c d]))]
          (recur r c m d))))))

(println "q1" (q1 312051))
(println "q2" (q2 312051))
