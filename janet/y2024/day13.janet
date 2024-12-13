#!/usr/bin/env janet
#
# Claw Contraption

(import ../running)

# ================ helpers ================

(def coord-line-peg
  '{:dig (range "09")
    :num (some :dig)
    :main (sequence (thru ": X") (+ "+" "=") (number :num)
                    (thru ", Y") (+ "+" "=") (number :num))})

(defn read-machines
  [lines &opt prize-offset]
  (def parse-xy (partial peg/match coord-line-peg))
  (seq [[ba bb prize] :in (partition 4 lines)]
       (def [bax bay] (parse-xy ba))
       (def [bbx bby] (parse-xy bb))
       (def [px py] (parse-xy prize))
       {:a {:x bax :y bay}
        :b {:x bbx :y bby}
        :prize {:x (+ px (or prize-offset 0))
                :y (+ py (or prize-offset 0))}}))

(defn min-score
  "Return the min score needed to get the prize, or nil if it's not possible."
  [machine & max-presses]
  (var min-score nil)
  (let [ax ((machine :a) :x) ay ((machine :a) :y)
        bx ((machine :b) :x) by ((machine :b) :y)
        px ((machine :prize) :x) py ((machine :prize) :y)
        prize [px py]
        max-a-presses (min (or max-presses math/int-max) (inc (/ px ax)) (inc (/ py ay)))
        max-b-presses (min (or max-presses math/int-max) (inc (/ px bx)) (inc (/ py by)))]
    (loop [na :range [0 max-a-presses]
           nb :range [0 max-b-presses]
           :when (= prize [(+ (* ax na) (* bx nb))
                           (+ (* ay na) (* by nb))])]
      (def score (+ (* na 3) nb))
      (when (or (nil? min-score) (< score min-score))
        (set min-score score))))
  min-score)

# ================ part 1 ================

(defn part1 [lines]
  (def machines (read-machines lines))
  (+ ;(filter identity (map |(min-score $ 100) machines))))

# ================ part 2 ================

(defn part2 [lines]
  (def machines (read-machines lines 10000000000000))
  (+ ;(filter identity (map min-score machines))))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 13 true))
