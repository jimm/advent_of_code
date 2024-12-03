#!/usr/bin/env janet
#
# Mull It Over

(import ../running :as running)
(import ../util :as util)

(def mul-peg
  '{:dig (range "09")
    :num (between 1 3 :dig)
    :main (sequence "mul(" :num "," :num ")")})
(def do-peg '{:main "do()"})
(def dont-peg '{:main "don't()"})

# ================ helpers ================

(defn mul
  "Performs the multiplication instruction found at loc in line."
  [line loc]
  (var mul-nums (slice line (+ loc 4) (string/find ")" line loc)))
  (* ;(map util/stoi (string/split "," mul-nums))))

(defn sum-of-muls
  "Finds all mul() commands, runs them, and returns the sum."
  [line]
  (def locs (peg/find-all mul-peg line))
  (+ ;(map (fn [loc] (mul line loc)) locs)))

(defn find-previous
  "Returns the loc in if-locs before loc, or -1 if there isn't one."
  [if-locs loc]
  (def found (find (fn [[_ i]] (< i loc))
                   (reverse if-locs)
                   [:default -1]))
  (last found))

(defn enabled?
  "Returns true if the mul() at loc is enabled."
  [do-locs dont-locs loc]
  (def prev-do (find-previous do-locs loc))
  (def prev-dont (find-previous dont-locs loc))
  (> prev-do prev-dont))

(defn sum-of-enabled-muls
  "Finds all enabled mul() commands, runs them, and returns the sum."
  [line]
  # insert 0 at beginning of do-locs so we start "on"
  (def do-locs (map (fn [i] [:on i])
                    (array/insert (peg/find-all do-peg line) 0 0)))
  (def dont-locs (map (fn [i] [:off i])
                      (peg/find-all dont-peg line)))
  (def mult-allowed? (partial enabled? do-locs dont-locs))
  (def mult-locs (filter mult-allowed?
                         (peg/find-all mul-peg line)))
  (+ ;(map (fn [loc] (mul line loc)) mult-locs)))

# ================ part 1 ================

(defn part1 [lines]
  (sum-of-muls (string/join lines)))

# ================ part 2 ================

(defn part2 [lines]
  (sum-of-enabled-muls (string/join lines)))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 3))
