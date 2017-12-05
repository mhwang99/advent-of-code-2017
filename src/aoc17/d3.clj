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
  (loop [r 100
         c (inc r)
         m (-> (mapv
                 (fn [_] (mapv (fn [_] 0) (range (* 2 r))))
                 (range (* 2 r)))
               (assoc-in [r r] 1))
         d :r]
    (let [v (+ (get-in m [(dec r) (dec c)])
               (get-in m [r (dec c)])
               (get-in m [(inc r) (dec c)])
               (get-in m [(dec r) c])
               (get-in m [(inc r) c])
               (get-in m [(dec r) (inc c)])
               (get-in m [r (inc c)])
               (get-in m [(inc r) (inc c)]))]
      (if (>= v x)
        v
        (let [m (assoc-in m [r c] v)
              [r c d] (case d
                        :r (if (= 0 (get-in m [(dec r) c]))
                             [(dec r) c :u]
                             [r (inc c) d])
                        :u (if (= 0 (get-in m [r (dec c)]))
                             [r (dec c) :l]
                             [(dec r) c d])
                        :l (if (= 0 (get-in m [(inc r) c]))
                             [(inc r) c :d]
                             [r (dec c) d])
                        :d (if (= 0 (get-in m [r (inc c)]))
                             [r (inc c) :r]
                             [(inc r) c d]))]
          (recur r c m d))))))

(println "q1" (q1 312051))
(println "q2" (q2 312051))
