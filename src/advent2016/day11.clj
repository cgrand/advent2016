(ns advent2016.day11)

(def day11-input "The first floor contains a thulium generator, a thulium-compatible microchip, a plutonium generator, and a strontium generator.
The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip.
The third floor contains a promethium generator, a promethium-compatible microchip, a ruthenium generator, and a ruthenium-compatible microchip.
The fourth floor contains nothing relevant.")

(defn valid-floor? [floor]
  (or (not-any? (fn [[type]] (= :gen type)) floor)
    (every? #{#{:gen} #{:gen :chip}} (map #(into #{} (map first) %) (vals (group-by second floor))))))  

(defn mission-accomplished? [[pos building]]
  (and (= 3 pos) (every? empty? (take 3 building))))

(defn day11-1 [input]
  (let [init
        (into []
             (map (fn [[_ items]]
                    (into #{} (for [[_ chip gen] (re-seq #"([a-z]+)-compatible microchip|([a-z]+) generator" items)]
                                [(if chip :chip :gen) (or chip gen)]))))
             (re-seq #"floor contains([^.]+)" input))
        step (fn [[pos building]]
               (let [floor (vec (nth building pos))
                     items-choices (for [i (range (count floor))
                                         j (range i (count floor))]
                                     [(nth floor i) (nth floor j)])
                     pos-choices (filter #(<= 0 % 3) [(inc pos) (dec pos)])]
                 (for [[a b] items-choices
                       pos' pos-choices
                       :let [building (-> building (update pos disj a b) (update pos' conj a b))]
                       :when (every? valid-floor? building)]
                   [pos' building])))
        step+ (fn [[states prev-states]]
                (let [#_#_prev-states (into prev-states states)]
                  [(doto (into #{} (comp (mapcat step) (remove prev-states)) states) (-> count (prn 'S)))
                   states]))]
    (count (take-while (comp #(not-any? mission-accomplished? %) first) (iterate step+ [#{[0 init]} #{}])))))

; no algo change for part II just ammended input, but it took a loooong time
(def day11-2-input "The first floor contains a thulium generator, a thulium-compatible microchip, a plutonium generator, and a strontium generator, 
an elerium generator, an elerium-compatible microchip, a dilithium generator, a dilithium-compatible microchip.
The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip.
The third floor contains a promethium generator, a promethium-compatible microchip, a ruthenium generator, and a ruthenium-compatible microchip.
The fourth floor contains nothing relevant.")