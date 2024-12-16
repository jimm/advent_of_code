#!/usr/bin/env janet
#
# Reindeer Maze

(import ../matrix :as mx)
(import ../running)

# ================ helpers ================

(def path (chr "."))
(def wall (chr "#"))
(def start (chr "S"))
(def end (chr "E"))

(defn read-maze [lines] (mx/from-lines lines))

(defn path-neighbors
  "Return locations of horizontal/vertical path cells of [r c]."
  [maze r c]
  (filter |(not= wall (mx/mget maze ;$))
          (map (fn [[dr dc]] [(+ r dr) (+ c dc)])
               [[0 1] [0 -1] [1 0] [-1 0]])))

(defn dead-end?
  [maze r c]
  (and (= path (mx/mget maze r c))
       (= 1 (length (path-neighbors maze r c)))))

(defn find-dead-ends
  [maze]
  (def dead-ends @[])
  (loop [[r row] :pairs maze
         [c cell] :pairs row
         :unless (= cell wall)
         :when (dead-end? maze r c)]
    (array/push dead-ends [r c]))
  dead-ends)

(defn dead-end-path-next
  "Given a dead end location, return the open path location next to it that
is also a part of the dead end. Return nil otherwise."
  [maze r c]
  (def next-path-loc (first (path-neighbors maze r c)))
  (when (and next-path-loc (dead-end? maze ;next-path-loc))
    next-path-loc))

(defn fill-dead-end
  [maze r c]
  (mx/mput maze r c wall)
  (var next-path-loc (dead-end-path-next maze r c))
  (while (and next-path-loc (= (mx/mget maze ;next-path-loc) path))
    (mx/mput maze ;next-path-loc wall)
    (set next-path-loc (dead-end-path-next maze ;next-path-loc))))

(defn remove-dead-ends
  [maze]
  (each dead-end (find-dead-ends maze)
    (when (= path (mx/mget maze ;dead-end)) # not start or end
      (fill-dead-end maze ;dead-end))))

(defn paths-from
  [maze loc end curr-path paths]
  (loop [p :in (path-neighbors maze ;loc)
         :when (not (find |(= p $) curr-path))]
    (if (= p end)
      (array/push paths (array ;curr-path p))
      (do
        (array/push curr-path p)
        (paths-from maze p end curr-path paths)
        (array/pop curr-path))))
  paths)

(defn find-all-paths
  [maze]
  (def start (mx/find-loc maze start))
  (def end (mx/find-loc maze end))
  (paths-from maze start end @[start] @[]))

(defn dir-from
  [loc1 loc2 curr-dir]
  (def dr (- (loc2 0) (loc1 0)))
  (def dc (- (loc2 1) (loc1 1)))
  (case curr-dir
    :east (case dr 0 :east 1 :north -1 :south)
    :west (case dr 0 :west 1 :north -1 :south)
    :north (case dc 0 :north 1 :east -1 :west)
    :south (case dc 0 :south 1 :east -1 :west)))

(defn score-path
  [path]
  (var score 0)
  (var dir :east)
  (var prev (first path))
  (each step (slice path 1)
    (def next-dir (dir-from prev step dir))
    (when (not= dir next-dir)
      (+= score 1000)
      (set dir next-dir))
    (set prev step)
    (+= score 1))
  score)

# ================ part 1 ================

(defn part1
  [lines]
  (def maze (read-maze lines))
  (remove-dead-ends maze)
  (printf "finding paths...")
  (def paths (find-all-paths maze))
  # could make this more efficient by stopping path scoring when it reaches the current minimum
  (printf "scoring...")
  (min ;(map score-path paths)))

# ================ part 2 ================

(defn part2
  [lines]
  )

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 16))
