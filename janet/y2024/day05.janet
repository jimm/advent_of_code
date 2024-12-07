#!/usr/bin/env janet
#
# Print Queue

(import ../running)
(import ../util)

# ================ helpers ================

(defn read-rules-and-updates
  [lines]
  (def rules-pairs (map |(map scan-number (string/split "|" $))
                        (take-while |(not (empty? $)) lines)))
  (var rules @{})
  (each pair rules-pairs
    (def [a b] pair)
    (unless (get rules a)
      (set (rules a) @[]))
    (array/push (rules a) b))
  (def updates-lines (drop 1 (drop-while |(not (empty? $)) lines)))
  (def updates (map |(map scan-number (string/split "," $)) updates-lines))
  [rules updates])

(defn update-ok?
  [rules update]
  (var ok true)
  (loop [i :range [0 (- (length update) 1)]
         j :range [(+ i 1) (length update)]
         :while ok]
    (def a (get update i))
    (def b (get update j))
    (def b-before (get rules b))
    (when (and b-before (util/in? b-before a))
      (set ok false)))
  ok)

(defn middle
  [xs]
  (get xs (/ (- (length xs) 1) 2)))

(defn reverse-rules
  [rules]
  (var rev-rules @{})
  (loop [[key vals] :pairs rules
         val :in vals]
    (unless (get rev-rules val)
      (set (rev-rules val) @[]))
    (array/push (rev-rules val) key))
  rev-rules)

(defn insert-page
  [back-rules page fixed]
  (def must-be-after (get back-rules page []))
  # Find index of first page that does not have to be before this one.
  (def i (find-index |(not (util/in? must-be-after $))
                     fixed))
  (array/insert fixed (or i -1) page))

(defn fix-update
  [rules update]
  (def back-rules (reverse-rules rules))
  (var fixed @[(first update)])
  (each page (slice update 1)
    (set fixed (insert-page back-rules page fixed)))
  fixed)

# ================ part 1 ================

(defn part1 [lines]
  (def [rules updates] (read-rules-and-updates lines))
  (def ok-updates
    (filter |(update-ok? rules $) updates))
  (+ ;(map middle ok-updates)))

# ================ part 2 ================

(defn part2 [lines]
  (def [rules updates] (read-rules-and-updates lines))
  (def before-rules (reverse-rules rules))
  (def not-ok-updates
    (filter |(not (update-ok? rules $)) updates))
  (+ ;(map |(middle (fix-update rules $)) not-ok-updates)))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 5 true))
