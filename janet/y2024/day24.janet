#!/usr/bin/env janet
#
# Crossed Wires

(import ../running)

# ================ helpers ================

(def name-to-func {"AND" band "OR" bor "XOR" bxor})
(def no-val -1)

(defn read-rules
  [lines]
  (def gates @{})
  (def rules @[])
  (each line lines
    (def words (string/split " " line))
    (if (= (chr ":") (last (first words)))
      (put gates (slice (first words) 0 -2) (scan-number (last words)))
      (array/push rules [(words 0)
                         (name-to-func (words 1))
                         (words 2)
                         (words 4)])))
  # Fill in gates that don't have initial values
  (each [g1 _ g2 g3] rules
    (if (not (has-key? gates g1)) (put gates g1 no-val))
    (if (not (has-key? gates g2)) (put gates g2 no-val))
    (if (not (has-key? gates g3)) (put gates g3 no-val)))
  [gates rules])

(defn find-rule-setting
  [gate rules]
  (find |(= gate ($ 3)) rules))

(defn solve-for
  [gate gates rules]
  # (printf "solve-for %j, gates %j" gate gates)
  (when (= (gates gate) no-val)
    (def rule (find-rule-setting gate rules))
    # (prin "  rule ") (pp rule)
    (def [g1 f g2 _] rule)
    (solve-for g1 gates rules)
    (solve-for g2 gates rules)
    (put gates gate (f (gates g1) (gates g2))))
  (gates gate))

(defn zs-to-number
  [zs gates]
  (var num 0)
  (each z zs
    (if (= 1 (gates z))
      (+= num (blshift 1 (scan-number (slice z 1))))))
  num)

# ================ part 1 ================

(defn part1
  [lines]
  (def [gates rules] (read-rules lines))
  (def zs (filter |(string/has-prefix? "z" $) (keys gates)))
  (each z zs
    (solve-for z gates rules))
  (zs-to-number zs gates))

# ================ part 2 ================

(defn part2
  [lines]
  )

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 24))
