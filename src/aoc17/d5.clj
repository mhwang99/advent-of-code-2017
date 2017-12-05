(ns aoc17.d5
  (:require [clojure.string :as s]))

(defn q0 [l fi]
  (let [cnt (count l)]
    (loop [l l
           i 0
           n 0]
      (if (>= i cnt)
        n
        (let [in (nth l i)]
          (recur (update l i (fi in))
                 (+ i in)
                 (inc n)))))))

(defn q1 [l]
  (q0 l (fn [_] inc)))

(defn q2 [l]
  (q0 l (fn [n] (if (>= n 3) dec inc))))

(def in
  (mapv #(Integer. %)
        (-> (slurp "resources/d5.txt")
            (s/split #"\n"))))

(q1 in)
(q2 in)
