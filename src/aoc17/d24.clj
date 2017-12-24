(ns aoc17.d24
  (:require [clojure.string :as s]))

(def in
  (mapv (fn [s]
          (set (mapv #(Integer. %) (s/split s #"/"))))
        (-> (slurp "resources/d24.txt")
            (s/split #"\n"))))

(defn q0 [sl]
   (loop [bs (->> (filter #(some #{0} %) sl)
                  (mapv #(assoc {}
                                :lst [%]
                                :nxt (apply + %)
                                :sum (apply + %))))
          mx (apply max (mapv :sum bs))]
     (let [nbs (mapcat (fn [{:keys [lst nxt sum]}]
                         (->> (filter #(and (not-any? #{%} lst) (% nxt)) sl)
                              (mapv (fn [s]
                                      (if (= 2 (count s))
                                        {:lst (conj lst s)
                                         :nxt (first (disj s nxt))
                                         :sum (+ sum (apply + s))}
                                        {:lst (conj lst s)
                                         :nxt nxt
                                         :sum (+ sum nxt nxt)})))))
                      bs)]
       (if (seq nbs)
         (recur nbs (apply max mx (mapv :sum nbs)))
         [mx (apply max (mapv :sum bs))]))))

(defn run []
  (let [ret (q0 in)]
    (println "q1" (first ret))
    (println "q2" (second ret))))

