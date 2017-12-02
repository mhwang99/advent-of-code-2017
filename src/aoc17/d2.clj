(ns aoc17.d2
  (:require [clojure.string :as s]))

(defn q1 [ll]
  (apply +
         (mapv (fn [l]
                 (- (apply max l)
                    (apply min l)))
               ll)))

(defn q2 [ll]
  (apply
    +
    (flatten
      (map (fn [l]
             (for [i l
                   j l
                   :when (and (not= i j)
                              (= 0 (mod j i)))]
               (/ j i)))
           ll))))

(def in
  (mapv (fn [l]
          (mapv #(Integer. %)
                (s/split l #"\t")))
        (-> (slurp "resources/d2.txt")
            (s/split #"\n"))))

(q1 in)
(q2 in)

