#!/usr/bin/env janet
#
# Hoof It

(import ../matrix :as mx)
(import ../running)
(import ../set)
(import ../util)

# ================ helpers ================

(def bzero (chr "0"))

(defn read-map
  [lines]
  (def m (mx/from-lines lines))
  # Turn the byte values into integers 0-9
  (loop [[r row] :pairs m
         [c cell] :pairs row]
    (mx/mput m r c (- cell bzero)))
  m)

(defn next-reachables
  "Return an array of locations with value val reachable from loc."
  [m loc val]
  (def [r c] loc)
  (def locs @[])
  (each [dr dc] [[-1 0] [0 1] [1 0] [0 -1]]
    (def loc [(+ r dr) (+ c dc)])
    (when (= val (mx/mget m ;loc))
      (array/push locs loc)))
  locs)

(defn gen-reachables
  "Return an array of tables of 0->1s, 2->2s, etc."
  [m]
  (def reachables (seq [_ :range [0 9]] @{}))
  (var locs (set/new ;(mx/find-locs m 0)))
  (loop [i :range [0 9]]
    (def next-locs (set/new))
    (each loc locs
      (def next-steps (next-reachables m loc (inc i)))
      (put (reachables i) loc next-steps)
      (set/add next-locs ;next-steps))
    (set locs next-locs))
  reachables)

(defn score
  [reachables zero-loc]
  # next-locs contains a list of locs that are reachable from the current
  # loc. We initialize it with the 1s reachably by zero-loc.
  (var next-locs (util/dig reachables 0 zero-loc))
  (loop [i :range [1 9]]
    (def new-next-locs (set/new))
    (loop [next :in next-locs]
      (set/add new-next-locs ;(util/dig reachables i next)))
    (set next-locs new-next-locs))
  (length next-locs))
  

(defn rating
  [reachables zero-loc]
  (var next-locs (util/dig reachables 0 zero-loc))
  (loop [i :range [1 9]]
    (def new-next-locs @[])
    (loop [next :in next-locs]
      (array/concat new-next-locs (util/dig reachables i next)))
    (set next-locs new-next-locs))
  (length next-locs))

# ================ part 1 ================

(defn part1 [lines]
  (var m (read-map lines))
  (var reachables (gen-reachables m))
  (+ ;(map |(score reachables $) (keys (reachables 0)))))

# ================ part 2 ================

(defn part2 [lines]
  (var m (read-map lines))
  (var reachables (gen-reachables m))
  (+ ;(map |(rating reachables $) (keys (reachables 0)))))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 10))
