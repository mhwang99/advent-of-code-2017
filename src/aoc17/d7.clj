(ns aoc17.d7
  (:require [clojure.string :as s]))

(defn q1 [ll]
  (let [[nl pm] (reduce (fn [[nl pm] l]
                          [(conj nl (first l))
                           (reduce (fn [m e]
                                     (assoc m e (first l)))
                                   pm (drop 2 l))])
                        [[] {}] ll)]
    (loop [nl nl]
      (if (get pm (first nl))
        (recur (rest nl))
        (first nl)))))

(defn q2 [ll]
  (let [[nm pm] (reduce (fn [[nm pm] l]
                          [(assoc nm (first l) (Integer. (second l)))
                           (reduce (fn [m e]
                                     (assoc m e (first l)))
                                   pm (drop 2 l))])
                        [{} {}] ll)
        rt (loop [nl (keys nm)]
             (if (get pm (first nl))
               (recur (rest nl))
               (first nl)))
        cm (reduce (fn [m l]
                     (assoc m (first l) (drop 2 l)))
                   {} ll)
        nnm (->> (reduce (fn [m [e v]]
                           (loop [p (get pm e)
                                  m m]
                             (if (nil? p) m
                               (recur (get pm p)
                                      (assoc m p (+ (get nm e) (get m p 0)))))))
                         {} nm)
                 (reduce (fn [m [e v]]
                           (update m e (partial + v)))
                         nm))
        bl? (fn [e]
              (if-let [cm (get cm e)]
                (apply = (mapv #(get nnm %) cm))
                true))]
    (loop [e rt]
      (let [es (filter (complement bl?) (get cm e))]
        (if (seq es)
          (recur (first es))
          (let [dm (group-by #(get nnm %) (get cm e))
                [n e] (loop [m dm]
                        (let [[n l] (first m)]
                          (if (= 1 (count l))
                            [n (first l)]
                            (recur (rest m)))))
                t (first (remove #(= % n) (keys dm)))]
            (+ (get nm e) (- t n))))))))

(def in
  (mapv #(re-seq #"\d+|\w+" %)
        (-> (slurp "resources/d7.txt")
            (s/split #"\n"))))

(defn run []
  (println "q1" (q1 in))
  (println "q2" (q2 in)))

