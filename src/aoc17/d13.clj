(ns aoc17.d13
  (:require [clojure.string :as s]))

(defn q1
  [dm]
  (reduce (fn [ret [i d]]
            (if (= 0 (mod i (+ d (- d 2))))
              (+ ret (* d i))
              ret))
          0 dm))

(defn q2
  [dm]
  (loop [n 0]
    (if (reduce (fn [_ [i d]]
                  (when (= 0 (mod (+ i n)
                                  (+ d (- d 2))))
                    (reduced i)))
                nil dm)
      (recur (inc n))
      n)))

(def in
  (into {} (mapv (fn [l]
          (mapv #(Integer. %) (re-seq #"\d+" l)))
        (-> (slurp "resources/d13.txt")
            (s/split #"\n")))))

(defn run []
  (println "q1" (q1 in))
  (println "q2" (q2 in)))
