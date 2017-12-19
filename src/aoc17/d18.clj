(ns aoc17.d18
  (:require [clojure.string :as s]))

(defn q1 [l]
  (let [getv (fn [m v]
               (if (number? v) v
                 (get m v 0)))]
    (loop [m {}
           i 0]
      (if (>= i (count l))
        (:p m)
        (let [[t a b] (nth l i)]
          (case t
            :set (recur (assoc m a (getv m b)) (inc i))
            :add (recur (assoc m a (+ (get m a 0) (getv m b))) (inc i))
            :mul (recur (assoc m a (* (get m a 0) (getv m b))) (inc i))
            :mod (recur (assoc m a (rem (get m a 0) (getv m b))) (inc i))
            :snd (recur (assoc m :p (get m a 0)) (inc i))
            :rcv (if (= 0 (get m a 0))
                   (recur m (inc i))
                   (:p m))
            :jgz (if (> (getv m a) 0)
                   (recur m (+ i (getv m b)))
                   (recur m (inc i)))))))))

(defn q2 [l]
  (let [getv (fn [m v]
               (if (number? v) v
                 (get m v 0)))
        genp (fn [m i s q n]
               {:m m :i i :s s :q q :n n})]
    (loop [pgm {false (genp {"p" 0} 0 :run [] 0)
                true  (genp {"p" 1} 0 :run [] 0)}
           p false]
      (let [{:keys [m i s q n]} (get pgm p)
            os (get-in pgm [(not p) :s])]
        (cond
          (or (= s os :end)
              (and (= s :blk) (empty? q))) (get-in pgm [true :n])
          (or (= s :end)
              (>= i (count l))) (recur (assoc pgm p (genp m i :end q n)) (not p))
          :else (let [[t a b] (nth l i)]
                  (if-let [m (case t
                               :set (assoc m a (getv m b))
                               :add (assoc m a (+ (get m a 0) (getv m b)))
                               :mul (assoc m a (* (get m a 0) (getv m b)))
                               :mod (assoc m a (rem (get m a 0) (getv m b)))
                               nil)]
                    (recur (assoc pgm p (genp m (inc i) :run q n)) p)
                    (case t
                      :jgz (if (> (getv m a) 0)
                             (recur (assoc pgm p (genp m (+ i (getv m b)) :run q n)) p)
                             (recur (assoc pgm p (genp m (inc i)          :run q n)) p))
                      :rcv (if (seq q)
                             (let [m (assoc m a (first q))
                                   q (vec (drop 1 q))]
                               (recur (assoc pgm p (genp m (inc i) :run q n)) p))
                             (recur (assoc pgm p (genp m i :blk q n)) (not p)))
                      :snd (let [v (getv m a)
                                 pgm (update-in pgm [(not p) :q] conj v)]
                             (recur (assoc pgm p (genp m (inc i) :run q (inc n))) p))))))))))

(def in
  (mapv (fn [s]
          (let [[a b c] (s/split s #" ")
                atoi #(try
                         (Integer. %)
                         (catch Exception _
                            %))
                b (atoi b)
                c (when c
                    (atoi c))]
             [(keyword a) b c]))
        (-> (slurp "resources/d18.txt")
              (s/split #"\n"))))

(defn run []
  (println "q1" (q1 in))
  (println "q2" (q2 in)))
