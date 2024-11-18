(ns day13
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn cmp
  ([[l r]]
   (cmp l r))
  ([l r]
   (cond
     (and (nil? l) (nil? r))           ::=
     (and (nil? l) (some? r))          ::<
     (and (some? l) (nil? r))          ::>
     (and (number? l) (sequential? r)) (cmp [l] r)
     (and (sequential? l) (number? r)) (cmp l [r])
     (and (number? l) (number? r))     (cond (< l r) ::< (> l r) ::> (= l r) ::=)
     :else                             (let [res (cmp (first l) (first r))]
                                         (if (= res ::=)
                                           (cmp (next l) (next r))
                                           res)))))

(defn solve1 [f]
  (->> f slurp str/split-lines (remove #{""}) (map edn/read-string) (partition-all 2)
       (map cmp) (keep-indexed (fn [i res] (when (= ::< res) (inc i)))) (reduce +)))

(-> "input13ex.txt" solve1 println)
(-> "input13.txt" solve1 println)
