#!/usr/bin/env janet
#
# LAN Party

(import ../running)
(import ../set)
(import ../util)

# ================ helpers ================

(defn read-connections
  [lines]
  (def conns @{})
  (each line lines
    (let [[c1 c2] (string/split "-" line)
          c1-conns (conns c1)
          c2-conns (conns c2)]
      (put conns c1 (array/push (or c1-conns @[]) c2))
      (put conns c2 (array/push (or c2-conns @[]) c1))))
  conns)

# ================ part 1 ================

(defn part1
  [lines]
  (def conns (read-connections lines))
  (var triplets (set/new))
  (var count 0)
  (loop [t :in (filter |(string/has-prefix? "t" $) (keys conns))
         :when (string/has-prefix? "t" t)
         c1 :in (filter |(not= t $) (conns t))
         c2 :in (conns c1)
         c3 :in (conns c2)
         :when (= t c3)]
    (set/add triplets (tuple ;(sort @[t c1 c2]))))
  (length triplets))

# ================ part 2 ================

(defn part2
  [lines]
  )

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 23))
