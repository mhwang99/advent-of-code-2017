(ns aoc17.d21
  (:require [clojure.string :as s]))

(def in
  (reduce (fn [m l]
            (let [[a b] (map #(map seq (s/split % #"/"))
                             (s/split l #" => "))
                  rot (fn [a _]
                        (apply map (comp reverse str) a))
                  allp (->> (mapcat (fn [p]
                                      (reductions rot p (range 3)))
                                    [a (map reverse a)])
                            (map #(flatten %)))
                  b (apply str (flatten b))]
              (into m (zipmap allp (repeat b)))))
          {} (-> (slurp "resources/d21.txt")
                 (s/split #"\n"))))

(defn split-board [b n]
  (let [cnt (int (Math/sqrt (count b)))
        r (/ cnt n)]
    (->> (partition r (partition n b))
         (apply mapcat concat)
         (partition (* n n)))))

(defn merge-board [bs]
  (let [n (int (Math/sqrt (count (first bs))))
        s (apply str bs)
        r (int (Math/sqrt (count s)))]
    (->> (partition r (partition n s))
         (apply mapcat concat))))

(defn q0 [rm n]
  (reduce (fn [b _]
            (->> (if (= (rem (count b) 2) 0) 2
                   3)
                 (split-board b)
                 (map #(get rm %))
                 merge-board))
          ".#...####" (range n)))

(defn q1 [rm]
  (->> (q0 rm 5)
       (filter #(= % \#))
       count))

(defn q2 [rm]
  (->> (q0 rm 18)
       (filter #(= % \#))
       count))

(defn run []
  (println "q1" (q1 in))
  (println "q2" (q2 in)))
