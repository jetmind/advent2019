(def input
  (->> (slurp "input.txt")
       (clojure.string/split-lines)
       (mapv #(Long/parseLong %))))


(defn fuel [mass]
  (- (quot mass 3) 2))


(defn solve []
  (reduce (fn [acc mass]
            (+ acc (fuel mass)))
    0 input))


(defn solve2 []
  (reduce (fn [acc mass]
            (->> (iterate fuel mass)
                 (next)
                 (take-while pos?)
                 (reduce +)
                 (+ acc)))
    0 input))


(println (solve))
(println (solve2))
