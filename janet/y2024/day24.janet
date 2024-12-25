#!/usr/bin/env janet
#
# Crossed Wires

(import ../running)
(import ../util)

# ================ helpers ================

(def name-to-func {"AND" band "OR" bor "XOR" bxor})
(def no-val -1)

(defn read-rules
  [lines]
  (def gates @{})
  (def rules @{})
  (each line lines
    (def words (string/split " " line))
    (if (= (chr ":") (last (first words)))
      (let [gate (slice (first words) 0 -2)
            val (scan-number (last words))]
        (put gates gate val))
      (let [[g1 fname g2 _ out] words]
        (put rules out [g1 (name-to-func fname) g2]))))

  # Fill in gates that don't have initial values
  (each gate (keys rules)
    (when (not (has-key? gates gate))
      (put gates gate no-val)))
  (each [g1 _ g2] (values rules)
    (when (not (has-key? gates g1)) (put gates g1 no-val))
    (when (not (has-key? gates g2)) (put gates g2 no-val)))

  [gates rules])

(defn solve-for
  [gate gates rules]
  # (printf "solve-for %j, gates %j" gate gates)
  (when (= (gates gate) no-val)
    (def rule (rules gate))
    # (prin "  rule ") (pp rule)
    (def [g1 f g2] rule)
    (put gates gate (f (solve-for g1 gates rules)
                       (solve-for g2 gates rules))))
  (gates gate))

(defn gates-with-prefix
  [prefix gates]
  (filter |(string/has-prefix? prefix $) (keys gates)))

(defn gates-to-number
  [prefix gates]
  (var num 0)
  (each gate (gates-with-prefix prefix gates)
    (if (= 1 (gates gate))
      (+= num (math/pow 2 (scan-number (slice gate 1))))))
  num)

# ================ part 1 ================

(defn part1
  [lines]
  (def [gates rules] (read-rules lines))
  (each gate (gates-with-prefix "z" gates)
    (solve-for gate gates rules))
  (gates-to-number "z" gates))

# ================ part 2 ================

(def find-swaps
  [sum gates rules zs]
  )

(defn part2
  [lines]
  (def [gates rules] (read-rules lines))
  (let [x (gates-to-number "x" gates)
        y (gates-to-number "y" gates)
        z (gates-to-number "z" gates)]
        swapped-wires (find-swaps (+ x y) gates rules zs)]
    (print (string/join (sort swapped-wires) ","))))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 24))
