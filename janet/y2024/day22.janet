#!/usr/bin/env janet
#
# Monkey Market

(import ../running)

# ================ helpers ================

(defn mix
  [a b]
  (bxor a b))

(defn prune
  [a]
  (% 16777216 a))

(defn next-secret
  [secret]
  (->> secret
       (partial * 64)
       (partial mix secret)
       prune
       (partial / 32)
       math/trunc
       (partial mix secret)))

# ================ part 1 ================

(defn part1
  [lines]
  )

# ================ part 2 ================

(defn part2
  [lines]
  )

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 22))
