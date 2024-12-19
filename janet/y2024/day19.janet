#!/usr/bin/env janet
#
# Linen Layout

(import spork/regex)
(import ../running)

# ================ helpers ================

(defn read-towel-data
  [lines]
  (def patterns (reverse (sort-by length (string/split ", " (first lines)))))
  (def designs (slice lines 1))
  [patterns designs])

(defn prefix?
  [p s]
  (def len (length p))
  (and (< len (length s))
       (= p (slice s 0 len))))

(defn possible?
  [ps s]
  (cond (zero? (length s)) true
        (find |(= $ s) ps) true
        (let [try-next (filter |(prefix? $ s) ps)]
          (find |(possible? ps (slice s (length $))) try-next))))

(defn num-pattern-combos
  [ps s]
  (cond (zero? (length s)) 1
        (find |(= $ s) ps) 1
        (let [try-next (filter |(prefix? $ s) ps)]
          (+ ;(map |(num-pattern-combos ps (slice s (length $)))
                   try-next)))))

# ================ part 1 ================

(defn part1
  [lines]
  (def [patterns designs] (read-towel-data lines))
  (length (filter |(possible? patterns $) designs)))

# ================ part 2 ================

(defn part2
  [lines]
  (def [patterns designs] (read-towel-data lines))
  (each design designs
    (printf "%j => %j" design (num-pattern-combos patterns design)))
  (+ ;(map |(num-pattern-combos patterns $) designs)))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 19))
