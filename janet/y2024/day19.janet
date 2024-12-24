#!/usr/bin/env janet
#
# Linen Layout

(import spork/regex)
(import ../running)
(use ../util)

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

(defmem num-pattern-combos
  [ps s]
  (cond
    (zero? (length s))
    0

    (find |(= $ s) ps)
    # yes this matches so that's one, but try the others, too
    (+ 1 (let [prefix-pats (filter |(and (not= $ s) (prefix? $ s)) ps)]
           (+ ;(map |(num-pattern-combos ps (slice s (length $)))
                    prefix-pats))))

    (let [prefix-pats (filter |(prefix? $ s) ps)]
      (+ ;(map |(num-pattern-combos ps (slice s (length $)))
               prefix-pats)))))

# ================ part 1 ================

(defn part1
  [lines]
  (def [patterns designs] (read-towel-data lines))
  (length (filter |(possible? patterns $) designs)))

# ================ part 2 ================

(defn part2
  [lines]
  (def [patterns designs] (read-towel-data lines))
  (+ ;(map |(num-pattern-combos patterns $) designs)))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 19))
