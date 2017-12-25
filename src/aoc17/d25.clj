(ns aoc17.d25
  (:require [clojure.string :as s]))

(def in
  (let [l (-> (slurp "resources/d25.txt")
              (s/split #"\n\n"))
        n (Integer. (first (re-seq #"\d+" (first l))))
        s (keyword (nth (s/split (first l) #" |\.") 3))
        m (reduce (fn [m s]
                    (let [l (mapv second (re-seq #" ([\w\d]+)[\.\:]\n?" s))]
                      (assoc m (keyword (nth l 0))
                             {false {:fvl (if (= (nth l 2) "1") conj disj)
                                     :mv (if (= "left" (nth l 3)) -1 1)
                                     :nxt (keyword (nth l 4))}
                              true {:fvl (if (= (nth l 6) "1") conj disj)
                                    :mv (if (= "left" (nth l 7)) -1 1)
                                    :nxt (keyword (nth l 8))}}))
                    )
                  {} (rest l))]
    [s n m]))

(defn q1 [[s n m]]
   (loop [n n
          p 0
          s s
          v #{}]
     (if (<= n 0)
       (count v)
       (let [{:keys [fvl mv nxt]} (get-in m [s (boolean (v p))])]
         (recur (dec n) (+ p mv) nxt (fvl v p))))))

(defn run []
  (println "q1" (q1 in)))
