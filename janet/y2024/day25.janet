#!/usr/bin/env janet
#
# Code Chronicle

(import ../matrix :as mx)
(import ../running)

# ================ helpers ================

(defn read-data
  [lines]
  (def locks @[])
  (def keys @[])
  (var height nil)
  (loop [obj-lines :in (partition-by empty? lines)
         :when (not (empty? (first obj-lines)))]
    (def lock? (= (chr "#") (first (first obj-lines))))
    (def obj-map (mx/from-lines (slice obj-lines 1)))
    (when (nil? height) (set height (dec (mx/height obj-map))))
    (def cols (seq [i :in (range 0 (mx/width obj-map))]
                   (mx/col obj-map i)))
    (if lock?
      (array/push locks (map (fn [col] (or (find-index |(= (chr ".") $) col)
                                           (length cols)))
                             cols))
      (array/push keys (map (fn [col] (- (length cols)
                                         (or (find-index |(= (chr "#") $) col)
                                             0)))
                            cols))))
  [height locks keys])

(defn fit?
  [height lock key]
  (all |(>= height (+ ;$))
       (partition 2 (interleave lock key))))

# ================ part 1 ================

(defn part1
  [lines]
  (def [height locks keys] (read-data lines))
  (+ ;(seq [lock :in locks
            key :in keys]
           (if (fit? height lock key) 1 0))))

# ================ part 2 ================

(defn part2
  [lines]
  )

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 25 true))
