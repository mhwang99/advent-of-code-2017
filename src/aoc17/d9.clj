(ns aoc17.d9
  (:require [clojure.string :as s]))

(defn q0 [s]
  (reduce (fn [[d l t n] e]
            (case d
              :n (cond
                   (= e \{) [d (inc l) t n]
                   (= e \}) [d (dec l) (+ t l) n]
                   (= e \<) [:g l t n]
                   :else [d l t n])
              :g (cond
                   (= e \!)  [:c l t n]
                   (= e \>)  [:n l t n]
                   :else [d l t (inc n)])
              :c [:g l t n]))
          [:n 0 0 0] s))

(defn q1 [s]
  (nth (q0 s) 2))

(defn q2 [s]
  (nth (q0 s) 3))

(def in (drop-last (slurp "resources/d9.txt")))

(defn run []
  (println "q1" (q1 in))
  (println "q2" (q2 in)))

