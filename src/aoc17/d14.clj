(ns aoc17.d14
  (:require [clojure.string :as s]))

(defn q0 [ll]
  (for [x (range 128)
        y (range 128)
        :when (= (get-in ll [x y]) \1)]
    [x y]))

(defn q1 [ll]
  (count (q0 ll)))

(defn q2 [ll]
  (aoc17.d12/q2
    (reduce (fn [sl [x y]]
              (let [[b sl] (reduce (fn [[b sl] s]
                                     (if (some s [[(dec x) y]
                                                  [(inc x) y]
                                                  [x (dec y)]
                                                  [x (inc y)]])
                                       [true (conj sl (conj s [x y]))]
                                       [b (conj sl s)]))
                                   [false []] sl)]
                (if b
                  sl
                  (conj sl #{[x y]}))))
            [] (q0 ll))))

(defn kh [s]
  (->> (concat (map int s) [17 31 73 47 23])
       (repeat 64)
       flatten
       (aoc17.d10/q0 256)
       (partition 16)
       (map #(->> (apply bit-xor %)
                  (Integer/toBinaryString)
                  (Integer/parseInt)
                  (format "%08d")))
       (apply str)))

(def in
  (let [s "ffayrhll"]
    (reduce (fn [ll i]
              (conj ll (vec (kh (str s "-" i)))))
            [] (range 128))))

(defn run []
  (println "q1" (q1 in))
  (println "q2" (q2 in)))

