(ns aoc17.d5
  (:require [clojure.string :as s]))

(defn q0 [l fi]
  (let [cnt (count l)]
    (loop [l l
           i 0
           n 0]
      (if (>= i cnt)
        n
        (let [o (nth l i)]
          (recur (update l i (fi o))
                 (+ i o)
                 (inc n)))))))

(defn q1 [l]
  (q0 l (fn [_] inc)))

(defn q2 [l]
  (q0 l #(if (>= % 3) dec inc)))

(def in
  (mapv #(Integer. %)
        (-> (slurp "resources/d5.txt")
            (s/split #"\n"))))

(defn run []
  (println "q1" (q1 in))
  (println "q2" (q2 in)))
