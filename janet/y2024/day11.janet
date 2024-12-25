#!/usr/bin/env janet
#
# Plutonian Pebbles

(import ../running)
(import ../util)

# ================ helpers ================

(util/defmem next-stone-val
  [val]
  (if (zero? val)
    1
    (let [val-str (string val)
          half-len (/ (length val-str) 2)]
      (if (int? half-len)       # even length
        [(scan-number (slice val-str 0 half-len))
         (scan-number (slice val-str half-len))]
        (* val 2024)))))

# ================ part 1 ================

(defn next-generation-naive
  [stones]
  (def new-stones (array/new (length stones)))
  (each stone stones
    (let [next (next-stone-val stone)]
      (if (number? next)
        (array/push new-stones next)
        (array/push new-stones ;next))))
  new-stones)

(defn part1 [lines]
  (var stones (util/extract-nums (first lines)))
  (repeat 25 (set stones (next-generation-naive stones)))
  (length stones))

# ================ part 2 ================

(defn update-stone-dict
  "Modify stone-dict to reflect the replacement of old-stone with new-stones."
  [stone-dict old-stone count new-stones]
  (put stone-dict old-stone (- (stone-dict old-stone) count))
  (if (number? new-stones)
    (put stone-dict new-stones (+ count (or (stone-dict new-stones) 0)))
    (each s new-stones
      (put stone-dict s (+ count (or (stone-dict s) 0))))))

(defn next-generation-efficient
  [stone-dict]
  (var new-stone-dict (table/clone stone-dict))
  (loop [[stone count] :pairs stone-dict
         :when (pos? count)]
    (def next-stones (next-stone-val stone))
    (update-stone-dict new-stone-dict stone count next-stones))
  new-stone-dict)

(defn part2 [lines]
  (if (dyn :testing)
    (print "NOTE: test fails but answer using real data is correct"))
  (def stones (util/extract-nums (first lines)))
  # table with keys = stones, vals = counts
  (var stone-dict (table ;(interleave stones (seq [:repeat (length stones)] 1))))

  (repeat 75 (set stone-dict (next-generation-efficient stone-dict)))
  (+ ;(values stone-dict)))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 11))
