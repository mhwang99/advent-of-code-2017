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
  (some (fn [n]
          (when-not (some (fn [[i d]]
                            (= 0 (mod (+ i n)
                                      (+ d (- d 2)))))
                          dm)
            n))
        (range)))

(def in
  (mapv (fn [l]
          (mapv #(Integer. %) (re-seq #"\d+" l)))
        (-> (slurp "resources/d13.txt")
            (s/split #"\n"))))

(defn run []
  (println "q1" (q1 in))
  (println "q2" (q2 in)))
