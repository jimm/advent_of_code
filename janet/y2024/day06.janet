#!/usr/bin/env janet
#
# Guard Gallivant

(import ../matrix)
(import ../running)
(import ../util)

# ================ helpers ================

(def obstacle (chr "#"))
(def guard (chr "^"))
(def dir-offsets
  {:up [-1 0]
   :down [1 0]
   :left [0 -1]
   :right [0 1]})
(def right-turns
  {:up :right
   :right :down
   :down :left
   :left :up})

(defn load-map
  [lines]
  (matrix/from-lines lines))

(defn mark-visited
  "Adds dir to array at cell location."
  [map r c dir]
  (def cell (matrix/mget map r c))
  (if (= (type cell) :array)
    (unless (util/includes? cell dir)
      (matrix/mput map r c (array/push cell dir)))
    (matrix/mput map r c @[dir])))

(defn visited?
  [map r c]
  (= (type (matrix/mget map r c)) :array))

(defn visited-same-dir?
  [map r c dir]
  (and (visited? map r c)
       (util/includes? (matrix/mget map r c) dir)))

(defn count-visited
  "Returns number of cells that were visited."
  [map]
  (def dirs [:up :down :left :right])
  (var count 0)
  (loop [r :range [0 (matrix/height map)]
         c :range [0 (matrix/width map)]]
    (if (visited? map r c)
      (++ count)))
  count)

(defn trace-guard-path
  "Traces the guard's path, returning a tuple containing a copy of the map
with each visited cell containing an array of directions the guard used to
pass through it and a symbol stating if the guard ended up :out-of-bounds or
:stuck-in-a-loop."
  [orig-map]
  (var m (matrix/copy orig-map))
  (var [r c] (matrix/find m guard))
  (var dir :up)
  (var done false)
  (while (and (not done)
              (not (visited-same-dir? m r c dir)))
    (mark-visited m r c dir)
    (def [dr dc] (dir-offsets dir))
    (+= r dr)
    (+= c dc)
    (if (matrix/in-bounds? m r c)
      (do
        (def goal (matrix/mget m r c))
        (when (= goal obstacle)
          (set dir (right-turns dir))
          (-= r dr)
          (-= c dc)))
      # out of bounds
      (set done true)))
  [m (if done :out-of-bounds :stuck-in-a-loop)])

(defn ok-to-place-obstacle?
  [m r c]
  (def cell (matrix/mget m r c))
  (not (util/includes? [obstacle guard] cell)))

(defn count-loop-obstructions
  [orig-map]
  (var count 0)
  (loop [r :range [0 (matrix/height orig-map)]
         c :range [0 (matrix/height orig-map)]
         :when (ok-to-place-obstacle? orig-map r c)]
    (var m (matrix/copy orig-map))
    (matrix/mput m r c obstacle)
    (def [_ reason] (trace-guard-path m))
    (when (= reason :stuck-in-a-loop)
      (++ count)))
  count)

# ================ part 1 ================

(defn part1 [lines]
  (def m (load-map lines))
  (def [m _] (trace-guard-path m))
  (count-visited m))

# ================ part 2 ================

(defn part2 [lines]
  (def m (load-map lines))
  (count-loop-obstructions m))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 6))
