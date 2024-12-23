#!/usr/bin/env janet
#
# Keypad Conundrum

(import ../running)

# ================ helpers ================

(def A 10)

(def dir-paths
  []
  {[A u] [l]
   [A r] [d]
   [A d] [[l d] [d l]]
   [A l] [[l d l] [d l l]]
   [u a] [r]
   [u 

(def num-paths
  []
  {[A 3] [:up]
   [A 6] [:up :up]
   [A 9] [:up :up]
   [A 0] [:left]
   [A 2] [[:left :up] [:up :up]]
   [A 5] [[l u u] [u l u] [
     

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
  (running/run-main part1 part2 2024 21))
