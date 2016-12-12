(ns advent2016.day12)

(def day12-input "cpy 1 a
cpy 1 b
cpy 26 d
jnz c 2
jnz 1 5
cpy 7 c
inc d
dec c
jnz c -2
cpy a c
inc a
dec b
jnz b -2
cpy c b
dec d
jnz d -6
cpy 14 c
cpy 14 d
inc a
dec d
jnz d -2
dec c
jnz c -5")

(defn day12-1 [input]
  (let [ops (vec (re-seq #"(cpy|jnz|inc|dec) (?:([abcd])|([+-]?\d+))(?: ([abcd])| ([+-]?\d+))?" input))
        step (fn [[pc regs]]
               (let [[_ op rega imma regb immb] (ops pc)]
                 (case op
                   "cpy" [(inc pc) (assoc regs regb (if rega (regs rega) (Long/parseLong imma)))]
                   "inc" [(inc pc) (update regs rega inc)]
                   "dec" [(inc pc) (update regs rega dec)]
                   "jnz" (if (zero? (if rega (regs rega) (Long/parseLong imma)))
                           [(inc pc) regs]
                           [(+ pc (Long/parseLong immb)) regs]))))]
    ops
    (some (fn [[pc regs]] (when (= pc (count ops)) regs)) (iterate step [0 {"a" 0 "b" 0 "c" 0 "d" 0}]))))

(defn day12-2 [input]
  (let [ops (vec (re-seq #"(cpy|jnz|inc|dec) (?:([abcd])|([+-]?\d+))(?: ([abcd])| ([+-]?\d+))?" input))
        step (fn [[pc regs]]
               (let [[_ op rega imma regb immb] (ops pc)]
                 (case op
                   "cpy" [(inc pc) (assoc regs regb (if rega (regs rega) (Long/parseLong imma)))]
                   "inc" [(inc pc) (update regs rega inc)]
                   "dec" [(inc pc) (update regs rega dec)]
                   "jnz" (if (zero? (if rega (regs rega) (Long/parseLong imma)))
                           [(inc pc) regs]
                           [(+ pc (Long/parseLong immb)) regs]))))]
    ops
    (some (fn [[pc regs]] (when (= pc (count ops)) regs)) (iterate step [0 {"a" 0 "b" 0 "c" 1 "d" 0}]))))