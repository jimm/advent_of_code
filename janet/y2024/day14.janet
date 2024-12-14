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
  # (pp quadrant-counts)          #WRONG! --- doesn't match locs I print in part1
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
  (pp locs)
  (def m (mx/from-size height width (chr ".")))
  (loop [[i [x y]] :pairs locs
         :let [val (mx/mget m y x)]]
    # (mx/mput m y x (+ (chr "a") i)))
    (mx/mput m y x (chr "*")))
  (unless no-gap
    (def middle-x (/ (dec width) 2))
    (def middle-y (/ (dec height) 2))
    (loop [r :range [0 height]]
      (mx/mput m r middle-x (chr " ")))
    (loop [c :range [0 width]]
      (mx/mput m middle-y c (chr " "))))
  (mx/pp m))

# (defn max-run
#   "Return [start length] of max run in row i of locs."
#   [locs i]
#   (def row (sorted-by last (filter |(= i ($ 1)))))
#   (
  

# (defn tree-formation?
#   [locs width height]
#   (def run-len nil)
#   (def run-start nil)
#   (loop [i :down [(dec height) -1]]
#     (def run (max-run locs i)))
#     (if (nil? run-len)      # looking for bottom row
#       (if (> row-max-run-len 20)
#         (run-len 

(defn part2 [lines]
)
  # (def robots (read-robots lines))
  # (def [width height] [101 103])
  # (var generation 1)
  # (while true
  #   (def locs (map |(run-robot $ width height i) robots))
  #   (when (tree-formation? locs width height)
  #     (printf "\e[2J================ %d ================" generation)
  #     (show-locs locs width height true)
  #     (getline))
  #   (+= generation 1)))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 14))
