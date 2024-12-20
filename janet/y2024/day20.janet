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
                                   (if (> (length path) 3) (slice path -3) path)))))
  (while (not= loc end)
    (set loc (first (filter not-seen (maze/path-neighbors maze ;loc))))
    (array/push path loc))
  path)

# ================ part 1 ================

(defn part1
  [lines]
  (def maze (maze/from-lines lines))
  (def path (trace-path maze (mx/find-loc maze (chr "S")) (mx/find-loc maze (chr "E"))))
  (def path-len (length path))
  (pp path-len)
  )

# ================ part 2 ================

(defn part2
  [lines]
  )

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 20))
