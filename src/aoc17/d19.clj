(ns aoc17.d19
  (:require [clojure.string :as s]))

(defn q0 [ll]
  (let [rev-dir (zipmap [:d :u :l :r] [:u :d :r :l])
        next-pnt (fn [[x y] d]
                   (case d
                     :d [x (inc y)]
                     :u [x (dec y)]
                     :l [(dec x) y]
                     :r [(inc x) y]))
        getc (fn [[nx ny]] (nth (nth ll ny) nx))
        next-dir (fn [p d]
                   (some #(when (and (not= (d rev-dir) %)
                                     (not= \space (getc (next-pnt p %))))
                            %)
                         [:d :u :l :r]))
        x (some #(when (= \| (getc [% 0])) %) (range))]
    (loop [p [x 0]
           d :d
           hs [\|]]
      (let [np (next-pnt p d)
            c (getc np)]
        (cond
          (= c \space) hs
          (= c \+) (recur np (next-dir np d) (conj hs c))
          :else (recur np d (conj hs c)))))))

(defn q1 [ll]
  (apply str
         (filter #(<= (int \A) (int %) (int \Z))
                 (q0 ll))))

(defn q2 [ll]
  (count (q0 ll)))

(def in
  (mapv #(vec %)
        (-> (slurp "resources/d19.txt")
            (s/split #"\n"))))

(defn run []
  (println "q1" (q1 in))
  (println "q2" (q2 in)))
