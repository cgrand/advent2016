(ns advent2016.day13)

(def input 1350)

(defn maze [magic-num]
  (fn [[x y]]
    (and (<= 0 x) (<= 0 y) 
      (even? (Long/bitCount (+ (* x x) (* 3 x) (* 2 x y) y (* y y) magic-num))))))

(defn part1 [input dest]
  (let [maze (maze input)
        step (fn [[curr visited]]
               [(into #{} (comp (filter maze) (remove visited)) (for [p curr, d [[0 1] [1 0] [0 -1] [-1 0]]] (mapv + p d)))
                (into visited curr)])]
    (count (take-while (fn [[curr]] (nil? (curr dest))) (iterate step [#{[1 1]} #{}])))))

(defn part2 [input]
  (let [maze (maze input)
        step (fn [[curr visited]]
               [(into #{} (comp (filter maze) (remove visited)) (for [p curr, d [[0 1] [1 0] [0 -1] [-1 0]]] (mapv + p d)))
                (into visited curr)])]
    (count (second (nth (iterate step [#{[1 1]} #{}]) 51)))))