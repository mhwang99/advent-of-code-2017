(ns aoc17.d22
  (:require [clojure.string :as s]))

(defn q0 [m cnt prm]
   (first
     (reduce (fn [[ret m d p] _]
               (let [v (get m p)
                     nv (get prm v)
                     nm (if nv
                          (assoc! m p nv)
                          (dissoc! m p))
                     nd (get-in {:u {nil :l   :in :r   :wk :u   :fl :d}
                                 :l {nil :d   :in :u   :wk :l   :fl :r}
                                 :d {nil :r   :in :l   :wk :d   :fl :u}
                                 :r {nil :u   :in :d   :wk :r   :fl :l}} [d v])
                     np (mapv #(%2 %1) p (nd {:l [dec identity]
                                              :r [inc identity]
                                              :d [identity inc]
                                              :u [identity dec]}))
                     ret (if (= nv :in)
                           (inc ret)
                           ret)]
                 [ret nm nd np]))
             [0 (transient m) :u [0 0]] (range cnt))))

(defn q1 [m]
  (q0 m 10000 {:in nil
                nil :in}))

(defn q2 [m]
  (q0 m 10000000 {nil :wk
                  :wk :in
                  :in :fl
                  :fl nil}))

(def in
  (let [b (mapv #(vec %)
                (-> (slurp "resources/d22.txt")
                    (s/split #"\n")))]
    (reduce (fn [m [x y]]
              (if (= \# (get-in b [y x]))
                (assoc m [(- x (int (/ (count b) 2))) (- y (int (/ (count b) 2)))] :in)
                m))
            {} (for [x (range (count b))
                     y (range (count b))]
                 [x y]))))

(defn run []
  (println "q1" (q1 in))
  (println "q2" (q2 in)))
