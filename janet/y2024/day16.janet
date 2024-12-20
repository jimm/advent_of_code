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
  [loc1 loc2 curr-dir]
  (def dr (- (loc2 0) (loc1 0)))
  (def dc (- (loc2 1) (loc1 1)))
  (case curr-dir
    :east (case dr 0 :east 1 :north -1 :south)
    :west (case dr 0 :west 1 :north -1 :south)
    :north (case dc 0 :north 1 :east -1 :west)
    :south (case dc 0 :south 1 :east -1 :west)))

(defn a-star
  [maze start end]
  (def came-from @{})
  (def oheap @[])
  (def gscore @{start 0})
  (def fscore @{start 0})
  (def close-set (set/new))
  (def came-from @{})
  (array/push oheap {:score 0 :loc start :dir :east})
  (def data @[])
  (while (and (not (empty? oheap)) (empty? data))
    #find oheap entry with min fscore
    (def min-fscore (min ;(map |($ :score) oheap)))
    (def i (find-index |(= min-fscore ($ :score)) oheap))
    (def fscore-and-cell (oheap i))
    (array/remove oheap i)
    (var current (fscore-and-cell :loc))
    (var dir (fscore-and-cell :dir))

    (if (= current end)
      (do
        (printf "end found")
        (pp current)
        (pp came-from)
        (pp (keys came-from))
        (pp (came-from current))
        (while (has-key? came-from current)
          (printf "found current in came-from, pushing current %j onto data %j" current data)
          (array/push data current)
          (set current (came-from current))))
      (do
        (set/add close-set current)
        (each p (maze/path-neighbors maze ;current)
          (def next-dir (dir-from current p dir))
          (def heur (if (= dir next-dir) 1001 1))
          (def tentative-g-score (+ (gscore current) heur))
          # (pp p)
          # (print "close-set contains p: " (close-set p))
          # (print ">= tent gscore: " (>= tentative-g-score (gscore p)))
          # (print "< tent gscore: " (< tentative-g-score (gscore p)))
          # (print "p not in oheap: " (nil? (find |(= $ p) (map |($ :loc) oheap))))

          (let [already-seen (close-set p)
                high-tentative-score (>= tentative-g-score (gscore p))
                oheap-has-neighbor (find |(= $ p) (map |($ :loc) oheap))]
            (when (and (not (and already-seen high-tentative-score))
                       (or (not high-tentative-score)
                           oheap-has-neighbor))
              (put came-from p current)
              (put gscore p tentative-g-score)
              (put fscore p (+ tentative-g-score heur))
              (array/push oheap {:score (+ heur (fscore-and-cell :score)) :loc p :dir next-dir})))))))
  (printf "data %j" data)
  data)

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
  (def maze (maze/from-lines lines))
  (maze/remove-dead-ends maze)
  (printf "finding paths...")

  (def path (a-star maze (mx/find-loc maze maze/start) (mx/find-loc maze maze/end)))
  (score-path path))

  # (def paths (find-all-paths maze))
  # could make this more efficient by stopping path scoring when it reaches the current minimum
  # (printf "scoring...")
  # (min ;(map score-path paths)))

# ================ part 2 ================

(defn part2
  [lines]
  )

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 16))
