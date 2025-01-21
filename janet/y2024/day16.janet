#!/usr/bin/env janet
#
# Reindeer Maze

(import ../matrix :as mx)
(import ../maze)
(import ../running)
(import ../set)

# ================ helpers ================

(defn dir-from
  "Return the direction needed to get from loc1 to loc2."
  [step loc2]
  (def dr (- (loc2 0) (get-in step [:loc 0])))
  (def dc (- (loc2 1) (get-in step [:loc 1])))
  (case (step :dir)
    :east (case dr 0 :east 1 :north -1 :south)
    :west (case dr 0 :west 1 :north -1 :south)
    :north (case dc 0 :north 1 :east -1 :west)
    :south (case dc 0 :south 1 :east -1 :west)))

# A step of a path is a tuple of the form {:loc loc :dir dir-sym :score score}
# A path is an array of steps.

(defn score-delta-and-dir-from
  "Return [delta-score new-direction] when moving from the end of `path` to `loc`."
  [path loc]
  (def last-step (last (path :steps)))
  (def new-dir (dir-from last-step loc))
  [(if (= new-dir (path :dir)) 1 1001) new-dir])

(defn paths-from
  "Fills `paths` with an array of all paths from `loc` to `end` with lowest
  scores and returns it."
  [maze loc end curr-path paths loc-min-scores]
  # optimization: neighbors will be close to the end
  (def path-end (if (> (length (curr-path :steps)) 2)
                  (slice (curr-path :steps) -3)
                  (curr-path :steps)))
  (loop [p :in (maze/path-neighbors maze ;loc)
         :when (not (find |(= p ($ :loc)) path-end))]
    (def [score-delta new-dir] (score-delta-and-dir-from curr-path p))
    (def new-score (+ (curr-path :score) score-delta))
    (def curr-min-score-at-p (or (loc-min-scores p) math/int-max))
    (if (>= new-score curr-min-score-at-p)
      (printf "score too high at %j, new-score %j curr-min-score-at-p %j" p new-score curr-min-score-at-p))
    (when (< new-score curr-min-score-at-p)
      (put loc-min-scores p new-score)
      (put curr-path :score new-score)
      (array/push (curr-path :steps) {:loc p :dir new-dir})
      (if (= p end)
        (array/push paths (table/clone curr-path))
        (do
          (paths-from maze p end curr-path paths loc-min-scores)
          (-= (curr-path :score) score-delta)
          (array/pop (curr-path :steps))))))
  paths)

(defn find-all-paths
  [maze &opt a b]
  "Return an array of all paths from a (default start) to b (default end)."
  (def start (or a (mx/find-loc maze maze/start)))
  (def end (or b (mx/find-loc maze maze/end)))
  (def maze-no-dead-ends (maze/remove-dead-ends (mx/copy maze)))
  (def paths @[])
  (def x
    (paths-from maze-no-dead-ends start end @{:score 0 :steps @[{:loc start :dir :east}]} paths @{})
    )
  paths)

# ================ part 1 ================

(defn part1
  [lines]
  (def maze (maze/from-lines lines))
  (maze/remove-dead-ends maze)

  (def paths (find-all-paths maze))
  (each path paths
    (print "PATH")
    (each step (path :steps)
      (printf "  %j" step)))
  (min ;(map |($ :score) paths)))

# ================ part 2 ================

(defn part2
  [lines]
  )

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 16))
