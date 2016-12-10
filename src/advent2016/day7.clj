(ns advent2016.day7)

(def day7-input
  (with-open [rdr (clojure.java.io/reader "src/advent2016/day7.input")]
    (vec (line-seq rdr))))

(defn day7-1 [input]
  (count (filter #(and (re-find #"([a-z])((?!\1)[a-z])\2\1" %)
                    (not (re-find #"\[[a-z]*([a-z])((?!\1)[a-z])\2\1[a-z]*\]" %))) input)))

(defn day7-2 [input]
  (count (filter #(or (re-find #"([a-z])((?!\1)[a-z])\1[a-z]*\[(?:\][a-z]*\[|[a-z])*\2\1\2" %)
                    (re-find #"([a-z])((?!\1)[a-z])\1[a-z]*\](?:\[[a-z]*\]|[a-z])*\2\1\2" %)) input)))