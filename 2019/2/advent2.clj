(def input
  (as-> (slurp "input.txt") $
        (clojure.string/split $ #",")
        (mapv #(Integer/parseInt %) $)
        (assoc $ 1 12)
        (assoc $ 2 2)))


(defn calc [input pointer]
  (let [[op x y z]   (drop pointer input)
        next-pointer (+ pointer 4)
        *x           (get input x)
        *y           (get input y)]
    (case op
      1  (recur (assoc input z (+ *x *y)) next-pointer)
      2  (recur (assoc input z (* *x *y)) next-pointer)
      99 input)))


(defn solve []
  (first (calc input 0)))


(defn solve2 []
  (->> (for [noun (range 100)
             verb (range 100)]
         (when (= 19690720 (first (calc (assoc input 1 noun 2 verb) 0)))
           (+ (* 100 noun) verb)))
       (remove nil?)
       (first)))


(println (solve))
(println (solve2))
