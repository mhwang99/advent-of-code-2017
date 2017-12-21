(ns aoc17.d21
  (:require [clojure.string :as s]))

(defn all-ptn [p]
  (let [n (count p)
        rot (fn [p _]
              (reduce (fn [o i]
                        (conj o (->> (flatten p)
                                     (drop i)
                                     (take-nth n)
                                     reverse
                                     vec)))
                      [] (range n)))
        rev (mapv (comp vec reverse) p)]
    (set (mapcat (fn [p]
                   (reductions rot p (range 3)))
                 [p rev]))))

(def in
  (mapv (fn [l]
          (mapv #(%2 %1)
                (mapv #(mapv vec (s/split % #"/"))
                      (s/split l #" => "))
                [all-ptn identity]))
        (-> (slurp "resources/d21.txt")
            (s/split #"\n"))))

(defn split-board [b n]
  (let [r (/ (count b) n)
        sb (partition n (flatten b))]
    (reduce (fn [o i]
              (concat o (->> (drop i sb)
                             (take-nth r)
                             (partition n))))
            [] (range r))))

(defn merge-board [bs]
  (let [n (count (first bs))
        r (* (int (Math/sqrt (count bs))) n)
        mb (partition n (flatten bs))]
    (->> (reduce (fn [o i]
                   (conj o (take-nth r (drop i mb))))
                 [] (range r))
         flatten
         (partition r))))

(defn q0 [rl n]
  (reduce (fn [b _]
            (->> (if (= (rem (count b) 2) 0) 2
                   3)
                 (split-board b)
                 (map (fn [b]
                        (some (fn [r]
                                (when ((first r) b)
                                  (second r))) rl)))
                 merge-board))
          (mapv vec [".#."
                     "..#"
                     "###"])
          (range n)))

(defn q1 [rl]
  (->> (q0 rl 5)
       flatten
       (filter #(= % \#))
       count))

(defn q2 [rl]
  (->> (q0 rl 18)
       flatten
       (filter #(= % \#))
       count))

(defn run []
  (println "q1" (q1 in))
  (println "q2" (q2 in)))
