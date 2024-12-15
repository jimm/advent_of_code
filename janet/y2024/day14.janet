#!/usr/bin/env janet
#
# Restroom Redoubt

(import ../running)
(import ../matrix :as mx)

# ================ helpers ================

(def robot-peg
  '{:dig (range "09")
    :num (* (? "-") (some :dig))
    :main (* (thru "p=") (number :num) "," (number :num)
             (thru  "v=") (number :num) "," (number :num))})

(defn read-robots
  [lines]
  (map (fn [line]
         (def [px py vx vy] (peg/match robot-peg line))
         {:p {:x px :y py} :v {:x vx :y vy}})
       lines))

(defn clamp-loc
  "Return loc wrapped around so it's inside width and height."
  [[x y] width height]
  (def mx (% x width))
  (def my (% y height))
  [(cond (>= x 0) mx
         (zero? mx) 0
         true (+ width mx))
   (cond (>= y 0) my
         (zero? my) 0
         true (+ height my))])

(defn run-robot
  [robot width height steps]
  (def loc [(+ (get-in robot [:p :x]) (* steps (get-in robot [:v :x])))
            (+ (get-in robot [:p :y]) (* steps (get-in robot [:v :y])))])
  (def [cx cy] (clamp-loc loc width height))
  (assert (and (>= cx 0) (< cx width)) (do (pp loc) (pp [cx cy]) "clamped x out of range"))
  (assert (and (>= cy 0) (< cy height)) (do (pp loc) (pp [cx cy]) "clamped y out of range"))
  (clamp-loc loc width height))

(defn safety-factor
  [locs width height]
  (def quadrant-counts @{:nw 0 :ne 0 :sw 0 :se 0})
  (def middle-x (/ (dec width) 2))
  (def middle-y (/ (dec height) 2))
  (def inc-quadrant (fn [q] (set (quadrant-counts q) (inc (quadrant-counts q)))))
  (each loc (filter (fn [[x y]] (and (not= middle-x x) (not= middle-y y))) locs)
    (def [x y] loc)
    (cond (> x middle-x) (cond (> y middle-y) (inc-quadrant :se)
                               (< y middle-y) (inc-quadrant :ne))
          (< x middle-x) (cond (> y middle-y) (inc-quadrant :sw)
                               (< y middle-y) (inc-quadrant :nw))))
  (* ;(values quadrant-counts)))

# ================ part 1 ================

(defn part1 [lines]
  (def robots (read-robots lines))
  (def [width height] (if (dyn :testing) [11 7] [101 103]))
  (def final-locs (map |(run-robot $ width height 100) robots))
  (safety-factor final-locs width height))

# ================ part 2 ================

(defn show-locs
  [locs width height &opt no-gap]
  (def m (mx/from-size height width (chr " ")))
  (loop [[i [x y]] :pairs locs
         :let [val (mx/mget m y x)]]
    (mx/mput m y x (chr "*")))
  (mx/pp m))

(defn near?
  [a b]
  (and (< (math/abs (- (a 0) (b 0))) 20)
       (< (math/abs (- (a 1) (b 1))) 20)))

(defn maybe-tree?
  [locs]
  (def xs (map first locs))
  (def ys (map last locs))
  (def avg-x (/ (sum (map first locs)) (length locs)))
  (def avg-y (/ (sum (map last locs)) (length locs)))
  (def threshold (/ (length locs) 4))
  (def num-near-avg (length (filter |(near? $ [avg-x avg-y]) locs)))
  (> num-near-avg threshold))

# This one must be run manually. Look for the tree!
(defn part2 [lines]
  (def robots (read-robots lines))
  (def [width height] [101 103])
  (var generation 1)
  (var done false)
  (var prev-tree-generation 0)
  (while (not done)
    (def locs (map |(run-robot $ width height generation) robots))
    (if (maybe-tree? locs)
      (do
        (printf "================ %d ================" generation)
        (show-locs locs width height true)
        (let [ch (first (getline))]
          (cond (= ch (chr "b")) (set generation prev-tree-generation)
                (= ch (chr "q")) (set done true)
                true (do
                       (set prev-tree-generation generation)
                       (+= generation 1))))))
    (+= generation 1)))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 14))
