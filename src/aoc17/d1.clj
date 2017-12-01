(ns aoc17.d1
  (:require [clojure.string :as s]))

(defn q1 [s]
  (let [n (count s)
        s (concat (seq s) s)]
    (reduce (fn [o i]
              (+ o (if (= (nth s i) (nth s (inc i)))
                     (- (int (nth s i)) 48)
                     0)))
            0 (range n))))

(defn q2 [s]
  (let [n (/ (count s) 2)
        s (concat (seq s) s)]
    (reduce (fn [o i]
              (+ o (if (= (nth s i) (nth s (- i n)))
                     (- (int (nth s i)) 48)
                     0)))
            0 (range n (* n 3)))))

(def input (drop-last (slurp "resources/d1.txt")))
(slurp "resources/d1.txt")

(q1 input)
(q2 input)
