(ns aoc17.d8
  (:require [clojure.string :as s]))

(defn q0 [ll]
  (reduce (fn [[m mx] [dest op v src cm cmv]]
            (if (cm (get m src 0) cmv)
              (let [nv (op (get m dest 0) v)]
                [(assoc m dest nv) (max mx nv)])
              [m mx]))
          [{} 0] ll))

(defn q1 [ll]
   (let [[m _] (q0 ll)]
     (apply max (vals m))))

(defn q2 [ll]
   (let [[_ mx] (q0 ll)]
     mx))

(defn parse [s]
  (let [[a b c _ d e f] (s/split s #" ")]
    [a
     (condp = b
       "inc" +
       "dec" -)
     (Integer. c)
     d
     (condp = e
       ">" >
       "<" <
       ">=" >=
       "==" =
       "<=" <=
       "!=" not=)
     (Integer. f)]))

(def in
  (mapv parse
        (-> (slurp "resources/d8.txt")
            (s/split #"\n"))))

(defn run []
  (println "q1" (q1 in))
  (println "q2" (q2 in)))

