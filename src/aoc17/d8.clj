(ns aoc17.d8
  (:require [clojure.string :as s]))

(defn q0 [ll]
  (reduce (fn [[m mx] [a fa va b fb vb]]
            (if (fb (get m b 0) vb)
              (let [nv (fa (get m a 0) va)]
                [(assoc m a nv) (max mx nv)])
              [m mx]))
          [{} 0] ll))

(defn q1 [ll]
  (let [[m _] (q0 ll)]
    (apply max (vals m))))

(defn q2 [ll]
  (let [[_ mx] (q0 ll)]
    mx))

(def opm {"inc" +
          "dec" -
          ">" >
          "<" <
          ">=" >=
          "==" =
          "<=" <=
          "!=" not=})
(def in
  (mapv #(let [[a b c _ d e f] (s/split % #" ")]
           [a (get opm b) (Integer. c)
            d (get opm e) (Integer. f)])
        (-> (slurp "resources/d8.txt")
            (s/split #"\n"))))

(defn run []
  (println "q1" (q1 in))
  (println "q2" (q2 in)))
