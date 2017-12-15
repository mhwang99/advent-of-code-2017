(ns aoc17.d15
  (:require [clojure.string :as s]))

(defn q0
  [ab cnt gv]
  (loop [n 0
         ab ab
         ret 0]
    (if (= n cnt)
      ret
      (let [[a b] (mapv (fn [a u m]
                          (reduce (fn [r v]
                                    (let [r (rem (* r v) 2147483647)]
                                      (if (= 0 (bit-and r (dec m)))
                                        (reduced r)
                                        r)))
                                  a (repeat u)))
                        ab [16807 48271] gv)]
        (if (= (bit-and a 65535) (bit-and b 65535))
          (recur (inc n) [a b] (inc ret))
          (recur (inc n) [a b] ret))))))

(defn q1 [ab cnt]
  (q0 ab cnt [1 1]))

(defn q2 [ab cnt]
  (q0 ab cnt [4 8]))

(def in
  (->> (slurp "resources/d15.txt")
       (re-seq #"\d+" )
       (mapv #(Integer. %))))

(defn run []
  (println "q1" (q1 in 40000000))
  (println "q2" (q2 in 5000000)))
