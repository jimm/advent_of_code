#!/usr/bin/env janet
#
# Race Condition

(import ../matrix :as mx)
(import ../maze)
(import ../running)

# ================ helpers ================

(defn trace-path
  "Assumes there is only one path through the maze."
  [maze start end]
  (var path @[start])
  (var loc start)
  (var not-seen (fn [p] (not (find |(= p $)
                                   # optimization: only need to check last two steps
                                   (if (> (length path) 3) (slice path -3) path)))))
  (while (not= loc end)
    (set loc (first (filter not-seen (maze/path-neighbors maze ;loc))))
    (array/push path loc))
  (array/push path end)
  path)

(defn paths-through-wall
  "Return the path or end locations that are through a wall next to [r c]."
  [maze r c]
  (filter identity
          (seq [[dr dc] :in [[0 1] [0 -1] [1 0] [-1 0]]
                :let [maybe-wall (mx/mget maze (+ r dr) (+ c dc))
                      maybe-path-loc [(+ r (* 2 dr)) (+ c (* 2 dc))]
                      maybe-path (mx/mget maze ;maybe-path-loc)]]
               (if (and (= maze/wall maybe-wall)
                        (or (= maze/path maybe-path) (= maze/end maybe-path)))
                 maybe-path-loc
                 nil))))

(defn num-cheat-paths-from
  "Return the number of times we could cut through a wall next to loc and
  shorten our total length by at >= :min-savings.

  Checking how much shorter the path is involves finding the nubmer of steps
  between loc and the step on the other side of the wall, then subtracting
  two because it takes two turns to get there."
  [maze path i loc]
  # Only check the steps after loc in the path
  (def path-after-loc (slice path (inc i)))
  # Eligible skip destinations
  (def ps (paths-through-wall maze ;loc))
  (+ ;(seq [p :in ps]
           (def p-idx (find-index |(= p $) path-after-loc))
           # We subtract 2 because it takes two turns to get there
           (if (and p-idx (>= (- p-idx 1) (dyn :min-savings))) 1 0))))

# ================ part 1 ================

(defn part1
  [lines]
  (setdyn :min-savings (if (dyn :testing) 2 100))
  (def maze (maze/from-lines lines))
  (def path (trace-path maze (mx/find-loc maze (chr "S")) (mx/find-loc maze (chr "E"))))
  (def path-len (length path))
  (var n 0)
  (loop [[i loc] :pairs (slice path 0 (- 0 (dyn :min-savings)))]
    (+= n (num-cheat-paths-from maze path i loc)))
  n)

# ================ part 2 ================

(defn part2
  (setdyn :min-savings (if (dyn :testing) 50 100))
  (def maze (maze/from-lines lines))
  (def path (trace-path maze (mx/find-loc maze (chr "S")) (mx/find-loc maze (chr "E"))))
  (def path-len (length path))
  (var n 0)
  (loop [[i loc] :pairs (slice path 0 (- 0 (dyn :min-savings)))]
    (+= n (num-cheat-paths-from maze path i loc)))
  n)

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 20))
