#!/usr/bin/env janet
#
# Plutonian Pebbles

(import ../running)
(import ../util)

# ================ helpers ================

(defn next-stone-val
  [val]
  (if (zero? val)
    1
    (let [val-str (string val)
          half-len (/ (length val-str) 2)]
      (if (int? half-len)       # even length
        [(scan-number (slice val-str 0 half-len))
         (scan-number (slice val-str half-len))]
        (* val 2024)))))

# (defn split-num
#   [val int-log10]
#   (let [div (math/pow 10 (/ (inc int-log10) 2))]
#     [(math/trunc (/ val div)) (% val div)]))

# (defn next-stone-val2
#   [val]
#   (if (zero? val)
#     1
#     (let [int-log10 (math/trunc (math/log10 val))]
#       (if (odd? int-log10)      # even number of digits
#         (split-num val int-log10)
#         (* val 2024)))))

(defn next-generation-naive
  [stones]
  (def new-stones (array/new (length stones)))
  (each stone stones
    (let [next (next-stone-val stone)]
      (if (number? next)
        (array/push new-stones next)
        (array/push new-stones ;next))))
  new-stones)

(def b-zero (chr "0"))
(def tuple-zero [b-zero])
(def tuple-one [(chr "1")])

(defn bytes-to-number
  [bytes]
  (scan-number (string/from-bytes ;bytes)))

(defn rm-leading-zeroes
  [bytes]
  (let [bs (drop-while |(= b-zero $) bytes)]
    (if (empty? bs) tuple-zero bs)))

(defn next-stone-bytes
  [stone]
  (if (= tuple-zero stone)
    tuple-one
    (let [half-len (/ (length stone) 2)]
      (if (int? half-len)       # even length
        [:split
         (slice stone 0 half-len)
         (rm-leading-zeroes (slice stone half-len))]
        (string/bytes (string (* 2024 (bytes-to-number stone))))))))

(defn next-generation-efficient
  [stones]
  (def new-stones (array/new (length stones)))
  (each stone stones
    (let [next (next-stone-bytes stone)]
      (if (= :split (next 0))
        (array/push new-stones ;(slice next 1))
        (array/push new-stones next))))
  new-stones)

# ================ part 1 ================

(defn part1 [lines]
  # (var stones (util/extract-nums (first lines)))
  # (repeat 25
  #         (set stones (next-generation-efficient stones)))
  # (length stones))
  (var stones (map string/bytes (util/words (first lines))))
  (repeat 25
          (set stones (next-generation-efficient stones)))
  (length stones))

# ================ part 2 ================

(defn part2 [lines]
  (var stones (map string/bytes (util/words (first lines))))
  (repeat 75
          (set stones (next-generation-efficient stones)))
  (length stones))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 11))
