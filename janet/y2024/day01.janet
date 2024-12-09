#!/usr/bin/env janet
#
# Historian Hysteria

(import ../running)
(import ../util)

# ================ helpers ================

(defn read-cols [lines]
  "Returns two columns of numbers."
  (let [pairs (map util/extract-nums lines)]
    (array (map first pairs) (map last pairs))))

# ================ part 1 ================

(defn part1 [lines]
  (let [[col1 col2] (read-cols lines)]
    (var both (interleave (sort col1) (sort col2)))
    (+ ;(seq [i :range [0 (length both) 2]]
             (math/abs (- (both i) (both (inc i))))))))

# ================ part 2 ================

(defn part2 [lines]
  (let [[col1 col2] (read-cols lines)
        freqs (frequencies col2)]
    (+ ;(map (fn [val] (* val (or (get freqs val) 0))) col1))))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 1))
