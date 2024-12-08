#!/usr/bin/env janet
#
# Resonant Collinearity

(import ../matrix :as mx)
(import ../running)

# ================ helpers ================

(def dot (chr "."))

(defn all-pairs
  [xs]
  (if (empty? xs)
    @[@[]]
    (do
      (def max (length xs))
      (var pairs @[])
      (loop [[i val] :pairs xs
             j :range [(inc i) max]]
        (array/push pairs [val (xs j)]))
      pairs)))

(defn find-antennas
  [m]
  (var antenna-locs @{})
  (loop [[r row] :pairs m
         [c antenna] :pairs row
         :when (not= dot antenna)]
    (when (nil? (antenna-locs antenna))
      (put antenna-locs antenna @[]))
    (array/push (get antenna-locs antenna) [r c]))
  antenna-locs)

(defn next-antinode-up
  [prev-an rs cs dr dc is-down-l-to-r?]
  (def col-op (if is-down-l-to-r? - +))
  [(- (prev-an 0) dr)
   (col-op (prev-an 1) dc)])

(defn next-antinode-down
  [prev-an rs cs dr dc is-down-l-to-r?]
  (def col-op (if is-down-l-to-r? + -))
  [(+ (prev-an 0) dr)
   (col-op (prev-an 1) dc)])

(defn single-antinodes
  [loc1 loc2]
  (def rs (map 0 [loc1 loc2]))
  (def cs (map 1 [loc1 loc2]))
  (def dr (math/abs (- ;rs)))
  (def dc (math/abs (- ;cs)))
  (def is-down-l-to-r? (< (cs 0) (cs 1)))
  (def first-higher? (< (loc1 0) (loc2 0)))

  [(next-antinode-up
    (if first-higher? loc1 loc2) rs cs dr dc is-down-l-to-r?)
   (next-antinode-down
    (if first-higher? loc2 loc1) rs cs dr dc is-down-l-to-r?)])

(defn in-bounds-antinodes
  [m loc1 loc2]
  (def rs (map 0 [loc1 loc2]))
  (def cs (map 1 [loc1 loc2]))
  (def dr (math/abs (- ;rs)))
  (def dc (math/abs (- ;cs)))
  (def is-down-l-to-r? (< (cs 0) (cs 1)))
  (def first-higher? (< (loc1 0) (loc2 0)))
  (var antinodes @[(if first-higher? loc1 loc2)
                   (if first-higher? loc2 loc1)])

  (var an (next-antinode-up (antinodes 0) rs cs dr dc is-down-l-to-r?))
  (while (mx/in-bounds? m ;an)
    (array/push antinodes an)
    (set an (next-antinode-up an rs cs dr dc is-down-l-to-r?)))

  (set an (next-antinode-down (antinodes 1) rs cs dr dc is-down-l-to-r?))
  (while (mx/in-bounds? m ;an)
    (array/push antinodes an)
    (set an (next-antinode-down an rs cs dr dc is-down-l-to-r?)))
  antinodes)

# ================ part 1 ================

(defn part1 [lines]
  (def m (mx/from-lines lines))
  (def antenna-locs (find-antennas m))
  # table: key = freq, val = seq of locs
  (def antinode-locs @{})
  (each locs (values antenna-locs)
    (each [loc1 loc2] (all-pairs locs)
      (let [[an1 an2] (single-antinodes loc1 loc2)]
        (when (mx/in-bounds? m ;an1)
          (put antinode-locs an1 true))
        (when (mx/in-bounds? m ;an2)
          (put antinode-locs an2 true)))))
  (length antinode-locs))

# ================ part 2 ================

(defn part2 [lines]
  (def m (mx/from-lines lines))
  (def antenna-locs (find-antennas m))
  # table: key = freq, val = seq of locs
  (def antinode-locs @{})
  (each locs (values antenna-locs)
    (each [loc1 loc2] (all-pairs locs)
      (each an (in-bounds-antinodes m loc1 loc2)
        (put antinode-locs an true))))
  (length antinode-locs))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 8))
