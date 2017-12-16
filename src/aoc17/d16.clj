(ns aoc17.d16
  (:require [clojure.string :as s]))

(defn q1 [s dl]
  (apply str
         (reduce (fn [l [t a b]]
                   (case t
                     :s (vec (concat (drop (- 16 a) l) (take (- 16 a) l)))
                     :x (assoc l a (l b) b (l a))
                     :p (let [ai (.indexOf l a)
                              bi (.indexOf l b)]
                          (assoc l ai (l bi) bi (l ai)))))
                 (vec s) dl)))

(defn q2 [s dl]
  (let [i (reduce (fn [l i]
                    (let [nl (q1 l dl)]
                      (if (= nl s)
                        (reduced (inc i))
                        nl)))
                  s (range 1000000000))]
    (if (string? i)
      i
      (let [cnt (mod 1000000000 i)]
        (reduce (fn [l _]
                  (q1 l dl))
                s (range cnt))))))

(def in
  (mapv (fn [s]
          (condp = (first s)
            \s [:s (Integer. (apply str (rest s))) nil]
            \x (into [:x] (map #(Integer. %) (re-seq #"\d+" s)))
            \p [:p (nth s 1) (nth s 3)]))
        (-> (slurp "resources/d16.txt")
            (s/split #"\n|,"))))

(def s0 "abcdefghijklmnop")

(defn run []
  (println "q1" (q1 s0 in))
  (println "q2" (q2 s0 in)))
