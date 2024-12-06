#!/usr/bin/env janet
#
# Guard Gallivant

(import ../matrix)
(import ../running)
(import ../util)

# ================ helpers ================

(def obstacle (chr "#"))
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

# ================ part 1 ================

(defn part1 [lines]
  (def map (load-map lines))
  (var [r c] (matrix/find map (chr "^")))
  (var dir :up)
  (var done false)
  (while (and (not done)
              (not (visited-same-dir? map r c dir)))
    (mark-visited map r c dir)
    (def [dr dc] (dir-offsets dir))
    (+= r dr)
    (+= c dc)
    (if (matrix/in-bounds? map r c)
      (do
        (def goal (matrix/mget map r c))
        (when (= goal obstacle)
          (set dir (right-turns dir))
          (-= r dr)
          (-= c dc)))
      (do
        (set done true)
        )))
  (count-visited map))

# ================ part 2 ================

(defn part2 [lines]
  )

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 6))
