(ns aoc17.d10
  (:require [clojure.string :as s]))

(defn q0 [cnt ds]
  (let [rev (fn [l c d]
              (let [nl (concat (drop c l) (take c l))
                    dl (reverse (take d nl))
                    nl (concat dl (drop d nl))]
                (concat (drop (- cnt c) nl) (take (- cnt c) nl))))]
    (first
      (reduce (fn [[l c s] d]
                [(rev l c d) (mod (+ c d s) cnt) (inc s)])
              [(range cnt) 0 0] ds))))

(defn q1 [s]
  (let [ds (mapv #(Integer. %) (s/split s #","))
        [a b] (q0 256 ds)]
    (* a b)))

(defn q2 [s]
  (->> (concat (map int s) [17 31 73 47 23])
       (repeat 64)
       flatten
       (q0 256)
       (partition 16)
       (map #(format "%02x" (apply bit-xor %)))
       (apply str)))

(def in
  (apply str (drop-last (slurp "resources/d10.txt"))))

(defn run []
  (println "q1" (q1 in))
  (println "q2" (q2 in)))
