(ns aoc17.d4
  (:require [clojure.string :as s]))


(defn q0 [ll cv]
  (count
    (filter (fn [l]
              (not-any?
                identity
                (for [i (range (count l))
                      j (range (count l))
                      :when (> j i)]
                  (= (cv (nth l i))
                     (cv (nth l j))))))
            ll)))

(defn q1 [ll]
  (q0 ll identity))

(defn q2 [ll]
  (q0 ll (comp set seq)))

(def in
  (mapv #(s/split % #" ")
        (-> (slurp "resources/d4.txt")
            (s/split #"\n"))))

(q1 in)
(q2 in)
