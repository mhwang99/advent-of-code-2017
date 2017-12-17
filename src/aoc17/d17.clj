(ns aoc17.d17
  (:require [clojure.string :as s]))

(defn q1 [x]
  (let [[l i] (reduce (fn [[l c] i]
                        (let [n (rem (+ x c 1) i)]
                          [(vec (concat (take n l) [i] (drop n l))) n])
                        )
                      [[0 1] 1] (range 2 2018))]
    (nth l (inc i))))

(defn q2 [x]
  (first
    (reduce (fn [[ret c] i]
              (let [n (inc (rem (+ x c) i))]
                (if (= n 1)
                  [i n]
                  [ret n])))
            [1 1] (range 2 50000001))))

(def in 337)

(defn run []
  (println "q1" (q1 in))
  (println "q2" (q2 in)))
