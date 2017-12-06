(ns aoc17.d6
  (:require [clojure.string :as s]))

(defn q0 [l q2?]
  (let [cnt (count l)]
    (reduce
      (fn [[l h] n]
        (if-let [i (get h l)]
          (reduced (if q2? (- n i) n))
          (let [mx (apply max l)
                i (.indexOf l mx)]
            [(reduce #(update %1 (mod (+ i 1 %2) cnt) inc)
                     (assoc l i 0) (range mx))
             (assoc h l n)])))
      [l {}] (range))))

(defn q1 [l]
  (q0 l false))

(defn q2 [l]
  (q0 l true))

(def in
  (mapv #(Integer. %)
        (-> (slurp "resources/d6.txt")
            (s/split #"\t|\n"))))

(defn run []
  (println "q1" (q1 in))
  (println "q2" (q2 in)))
