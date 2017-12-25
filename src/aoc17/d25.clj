(ns aoc17.d25
  (:require [clojure.string :as s]))

(def in
  (let [l (-> (slurp "resources/d25.txt")
              (s/split #"\n\n"))
        n (Integer. (first (re-seq #"\d+" (first l))))
        s (keyword (nth (s/split (first l) #" |\.") 3))
        m (reduce (fn [m s]
                    (let [l (mapv second (re-seq #" ([\w\d]+)[\.\:]\n?" s))
                          gen (fn [l]
                                {:fvl (if (= (nth l 0) "1") conj disj)
                                 :mv (if (= "left" (nth l 1)) -1 1)
                                 :nxt (keyword (nth l 2))})]
                      (assoc m (keyword (nth l 0))
                             {false (gen (drop 2 l))
                              true (gen (drop 6 l))})))
                  {} (rest l))]
    [s n m]))

(defn q1 [[s n m]]
  (-> (reduce (fn [[v p s] _]
                (let [{:keys [fvl mv nxt]} (get-in m [s (boolean (v p))])]
                  [(fvl v p) (+ p mv) nxt]))
              [#{} 0 s] (range n))
      first count))

(defn run []
  (println "q1" (q1 in)))
