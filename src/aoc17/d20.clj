(ns aoc17.d20
  (:require [clojure.string :as s]))


(defn q0
  [[pm vm am] cnt fn-col]
  (let [addm (fn [ma mb]
               (reduce (fn [m [k v]]
                         (assoc m k (mapv #(+ %1 %2)
                                          v (get mb k))))
                       {} ma))]
    (loop [pm pm
           vm vm
           n 0]
      (if (> n cnt)
        pm
        (let [vm (addm vm am)
              pm (addm pm vm)
              cl (fn-col pm)]
          (if (seq cl)
            (recur (apply dissoc pm cl) (apply dissoc vm cl) (inc n))
            (recur pm vm (inc n))))))))

(defn q1
  [ml cnt]
  (->> (q0 ml cnt (fn [_] nil))
       (sort-by (fn [[k v]]
                  (->> (map #(max % (- %)) v)
                       (apply +))))
       ffirst))

(defn q2
  [ml cnt]
  (let [fn-collide (fn [pm]
                     (->> (reduce (fn [m [k v]]
                                    (assoc m v (conj (get m v []) k)))
                                  {} pm)
                          vals
                          (filter #(> (count %) 1))
                          flatten))]
    (count (q0 ml cnt fn-collide))))

(def in
  (let [raw (-> (slurp "resources/d20.txt")
                (s/split #"\n"))
        ll (mapv (fn [l n]
                   [n (partition 3
                                 (mapv #(Integer. %)
                                       (re-seq #"-?\d+" l)))])
                 raw (range))]
    (reduce (fn [[pm vm am] [n [p v a]]]
              [(assoc pm n (vec p))
               (assoc vm n (vec v))
               (assoc am n (vec a))])
            [{} {} {}] ll)))


(defn run []
  (println "q1" (q1 in 1000))
  (println "q2" (q2 in 500)))
