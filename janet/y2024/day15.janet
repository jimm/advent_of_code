#!/usr/bin/env janet
#
# Warehouse Woes

(import ../matrix :as mx)
(import ../running)

# ================ helpers ================

(def map-syms
  {(chr "#") :wall
   (chr "O") :box
   (chr "@") :bot
   (chr ".") nil})

(def move-syms
  {(chr "<") :left
   (chr ">") :right
   (chr "^") :up
   (chr "v") :down})

(def move-offsets
  {:up [-1 0]
   :down [1 0]
   :left [0 -1]
   :right [0 1]})

(defn build-map
  [lines]
  (map (fn [line] (seq [ch :in line] (map-syms ch)))
       lines))

(defn build-moves
  [s]
  (map (fn [ch] (move-syms ch))
       (string/bytes s)))

(defn read-map-and-moves
  [lines]
  (def map-lines @[])
  (var moves "")
  (each line lines
    (if (= (line 0) (chr "#"))
      (array/push map-lines line)
      (set moves (string moves line))))

  [(build-map map-lines) (build-moves moves)])

(defn debug-print-map
  [m]
  (loop [row :in m
         :after (printf "")
         cell :in row]
    (prin (case cell
             nil "."
             :wall "#"
             :box "0"
             :bot "@"
             "?"))))

(defn move-thing
  [m me loc move-offset]
  (def next-loc [(+ (loc 0) (move-offset 0)) (+ (loc 1) (move-offset 1))])
  (def thing (mx/mget m ;next-loc))
  (case thing
    :wall loc
    nil (do
          (mx/mput m ;next-loc me)
          (mx/mput m ;loc nil)
          next-loc)
    # box: try to move it and if it moved we can move as well
    (let [thing-next-loc (move-thing m thing next-loc move-offset)]
      (if (= thing-next-loc next-loc)
        loc
        (do
          (mx/mput m ;next-loc me)
          (mx/mput m ;loc nil)
          next-loc)))))

(defn run-robot
  [m moves]
  (var robot-loc (mx/find-loc m :bot))
  (each move moves
    (set robot-loc (move-thing m :bot robot-loc (move-offsets move)))
    # (debug-print-map m)
    ))

(defn find-boxes
  [m]
  (seq [[r row] :pairs m
        [c cell] :pairs row
        :when (= cell :box)]
       [r c]))

(defn gps-val [loc] (+ (* 100 (loc 0)) (loc 1)))

# ================ part 1 ================

(defn part1
  [lines]
  (var [m moves] (read-map-and-moves lines))
  (run-robot m moves)
  (+ ;(map gps-val (find-boxes m))))

# ================ part 2 ================

(defn part2
  [lines]
  )

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 15))
