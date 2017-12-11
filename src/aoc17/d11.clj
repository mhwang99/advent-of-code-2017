(ns aoc17.d11
  (:require [clojure.string :as s]))

(defn steps [x y]
  (loop [n 0
         x (max x (- x))
         y (max y (- y))]
    (cond
      (= x y 0) n
      (= y 0) (+ n (/ x 3))
      (> x 0) (if (> (/ y 2) (/ x 3))
                (recur (+ n (/ x 3)) 0 (- y (* 2 (/ x 3))))
                (recur (+ n (/ y 2)) (- x (* 3 (/ y 2))) 0))
      :else (+ n (/ y 4)))))

(defn q0 [l]
  (reduce (fn [[x y mx] e]
            (let [[x y] (condp = e
                          "n" [x (+ y 4)]
                          "s" [x (- y 4)]
                          "ne" [(+ x 3) (+ y 2)]
                          "nw" [(- x 3) (+ y 2)]
                          "sw" [(- x 3) (- y 2)]
                          "se" [(+ x 3) (- y 2)])]
              [x y (max mx (steps x y))]))
          [0 0 0] l))

(defn q1 [l]
  (let [[x y] (q0 l)]
    (steps x y)))

(defn q2 [l]
  (let [[_ _ mx] (q0 l)]
    mx))

(def in
  (-> (slurp "resources/d11.txt")
      (s/split #",|\n")))

(defn run []
  (println "q1" (q1 in))
  (println "q2" (q2 in)))

