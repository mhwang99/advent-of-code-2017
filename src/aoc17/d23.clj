(ns aoc17.d23
  (:require [clojure.string :as s]))

(def in
  (mapv (fn [s]
          (let [[a b c] (s/split s #" ")
                atoi #(try
                        (Integer. %)
                        (catch Exception _
                          (keyword %)))
                b (atoi b)
                c (when c
                    (atoi c))]
            [(keyword a) b c]))
        (-> (slurp "resources/d23.txt")
            (s/split #"\n"))))

(defn q0 [l]
  (let [getv (fn [m v]
               (if (number? v) v
                 (get m v 0)))]
    (loop [m {}
           n {:set 0 :sub 0 :mul 0 :jnz 0}
           i 0]
      (if (>= i (count l))
        [n m]
        (let [[t a b] (nth l i)
              n (update n t inc)]
          (case t
            :set (recur (assoc m a (getv m b)) n (inc i))
            :sub (recur (assoc m a (- (get m a 0) (getv m b))) n (inc i))
            :mul (recur (assoc m a (* (get m a 0) (getv m b))) n (inc i))
            :jnz (if (not= (getv m a) 0)
                   (recur m n (+ i (getv m b)))
                   (recur m n (inc i)))))))))

(defn q1 [l]
  (:mul (first (q0 l))))

(defn q2 [l]
  (let [b (:b (second (q0 (conj (take 6 l) [:set :a 1]))))
        c (- b (nth (nth l 7) 2))
        n (- (nth (nth l 30) 2))
        oe (nth (nth l 10) 2)]
    (loop [b b
           e oe
           h 0]
      (cond
        (> b c) h
        (> (* e e) b) (recur (+ b n) oe h)
        (= 0 (mod b e)) (recur (+ b n) oe (inc h))
        :else (recur b (inc e) h)))))

(defn run []
  (println "q1" (q1 in))
  (println "q2" (q2 in)))
