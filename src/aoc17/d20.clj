(ns aoc17.d20
  (:require [clojure.string :as s]))


(defn q0
  [[pm vm am] cnt fn-col]
  (let [addm (fn [ma mb]
               (reduce (fn [m [k l]]
                         (->> (mapv + l (get mb k))
                              (assoc m k)))
                       {} ma))]
    (first
      (reduce (fn [[pm vm] _]
               (let [vm (addm vm am)
                     pm (addm pm vm)
                     cl (fn-col pm)]
                 (cond->> [pm vm]
                   (seq cl) (mapv #(apply dissoc % cl)))))
              [pm vm] (range cnt)))))

(defn q1
  [ml cnt]
  (->> (q0 ml cnt (fn [_] nil))
       (sort-by (fn [[k v]]
                  (->> (map #(max % (- %)) v)
                       (apply +))))
       ffirst))

(defn q2
  [ml cnt]
  (count
    (q0 ml cnt (fn [pm]
                 (->> (reduce (fn [m [k v]]
                                (assoc m v (conj (get m v []) k)))
                              {} pm)
                     vals
                     (filter #(> (count %) 1))
                     flatten)))))

(def in
  (let [raw (-> (slurp "resources/d20.txt")
                (s/split #"\n"))
        ll (mapv (fn [l n]
                   [n (->> (re-seq #"-?\d+" l)
                           (mapv #(Integer. %))
                           (partition 3))])
                 raw (range))]
    (reduce (fn [[pm vm am] [n [p v a]]]
              [(assoc pm n (vec p))
               (assoc vm n (vec v))
               (assoc am n (vec a))])
            [{} {} {}] ll)))

(defn run []
  (println "q1" (q1 in 1000))
  (println "q2" (q2 in 500)))
