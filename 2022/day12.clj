(ns day12
  (:require [clojure.string :as str]))

(defn parse [f]
  (->> f slurp str/split-lines (mapv #(mapv identity %))))

(defn inbounds? [grid [x y]]
  (let [xmax (-> grid first count dec)
        ymax (-> grid count dec)]
    (and (<= 0 x xmax) (<= 0 y ymax))))

(defn lookup [grid [x y]]
  (-> grid (get y) (get x)))

(defn lookupn [grid coord]
  (let [c (lookup grid coord)]
    (int ({\S \a \E \z} c c))))

(defn nextmoves [grid [x y :as coord]]
  (let [cur      (lookupn grid coord)
        possible [[x (inc y)] [x (dec y)] [(inc x) y] [(dec x) y]]]
    (->> possible (filter (partial inbounds? grid)) (filter #(<= (- (lookupn grid %) cur) 1)))))

(defn findall [grid char]
  (for [y     (-> grid count range)
        x     (-> grid first count range)
        :when (= char (lookup grid [x y]))]
    [x y]))

(defn minpath [grid start end]
  (loop [q    (conj clojure.lang.PersistentQueue/EMPTY [start, 0])
         seen #{start}]
    (let [[point n] (peek q)
          q'        (pop q)
          moves     (some->> point (nextmoves grid) (remove seen))
          seen'     (into seen moves)
          q'        (into q' (map (fn [m] [m (inc n)]) moves))]
      (cond
        (empty? q)    nil
        (= point end) n
        :else         (recur q' seen')))))

(defn solve1 [f]
  (let [grid  (parse f)
        start (first (findall grid \S))
        end   (first (findall grid \E))]
    (minpath grid start end)))

(defn solve2 [f]
  (let [grid   (parse f)
        starts (concat (findall grid \a) (findall grid \S))
        end    (first (findall grid \E))]
    (reduce min (keep #(minpath grid % end) starts))))

(println (solve1 "input12ex1.txt"))
(println (solve1 "input12.txt"))
(println (solve2 "input12ex1.txt"))
(println (solve2 "input12.txt"))

(comment
  (solve1 "input12ex1.txt")
  (solve1 "input12.txt")
  (solve2 "input12ex1.txt")
  (solve2 "input12.txt"))
