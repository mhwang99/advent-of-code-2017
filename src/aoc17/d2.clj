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
    (mapv (fn [l]
            (reduce
              (fn [o i]
                (reduce
                  (fn [n j]
                    (let [ni (nth l i)
                          nj (nth l j)]
                      (cond
                        (= n -2) n
                        (and (not= i j)
                             (= 0 (mod nj ni))) (if (> n -1) -2
                                                  (/ nj ni))
                        :else n)))
                  o (range (count l))))
              -1 (range (count l))))
          ll)))

(def in
  (mapv (fn [l]
          (mapv #(Integer. %)
                (s/split l #"\t")))
        (-> (slurp "resources/d2.txt")
            (s/split #"\n"))))

(q1 in)
(q2 in)

