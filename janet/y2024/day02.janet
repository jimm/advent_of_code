#!/usr/bin/env janet
#
# Year 2024, Day 2

(import ../running)
(import ../util)

# ================ helpers ================

(defn direction
  [a b]
  (cond (> a b) :down
     (< a b) :up
     :error))

(defn diff-ok?
  [dir a b]
  (var diff (- a b))
  (when (= dir :up)
    (set diff (- diff)))
  (and (> diff 0) (< diff 4)))

(defn safe?
  [report]
  (def dir (direction ;(slice report 0 2)))
  (var ok (not= dir :error))
  (loop [i :range [0 (- (length report) 1)]
         :while ok]
    (def pair (slice report i (+ i 2)))
    (unless (and (= (direction ;pair) dir)
                 (diff-ok? dir ;pair))
      (set ok false)))
  ok)

(defn all-but
  "Returns a copy of arr with element i removed."
  [arr i]
  (array/remove (array ;arr) i))

(defn safe-with-problem-dampener?
  [report]
  (if (safe? report)
    true
    (any? (map safe? (seq [i :range [0 (length report)]]
                          (all-but report i))))))

(defn read-reports
  [lines]
  (map util/extract-nums lines))

# ================ part 1 ================

(defn part1 [lines]
  (let [reports (read-reports lines)]
    (count safe? reports)))

# ================ part 2 ================

(defn part2 [lines]
  (let [reports (read-reports lines)]
    (count safe-with-problem-dampener? reports)))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 2))
