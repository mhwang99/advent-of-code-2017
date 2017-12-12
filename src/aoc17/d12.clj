(ns aoc17.d12
  (:require [clojure.string :as s]))

(defn q0 [ll]
  (loop [ll ll]
    (let [gsl (reduce
                (fn [gs l]
                  (loop [gs gs
                         ps []]
                    (cond
                      (empty? gs) (conj ps l)
                      (some (first gs) l) (concat
                                            ps
                                            [(into (first gs) l)]
                                            (rest gs))
                      :else (recur (rest gs) (conj ps (first gs))))))
                [] ll)]
      (if (= gsl ll)
        gsl
        (recur gsl)))))

(defn q1 [ll]
  (reduce (fn [_ s]
            (when (s 0)
              (reduced (count s))))
          nil (q0 ll)))

(defn q2 [ll]
  (count (q0 ll)))

(def in
  (mapv (fn [l]
          (set
            (mapv #(Integer. %) (re-seq #"\d+" l))))
        (-> (slurp "resources/d12.txt")
            (s/split #"\n"))))

(defn run []
  (println "q1" (q1 in))
  (println "q2" (q2 in)))
