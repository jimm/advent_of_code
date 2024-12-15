#!/usr/bin/env janet
#
# Warehouse Woes

(import ../matrix :as mx)
(import ../running)

# ================ helpers ================

(def map-syms
  {(chr "#") :wall
   (chr "O") :box
   (chr "[") :lbox
   (chr "]") :rbox
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

(defn find-boxes
  "Return array of box locations. Embiggened boxes return their left sides."
  [m]
  (seq [[r row] :pairs m
        [c cell] :pairs row
        :when (or (= cell :box) (= cell :lbox))]
       [r c]))

(defn debug-print-map
  [m]
  (loop [row :in m
         :after (printf "")
         cell :in row]
    (prin (case cell
             nil "."
             :wall "#"
             :box "0"
             :lbox "["
             :rbox "]"
             :bot "@"
             "?"))))

(defn d-loc
  "Return a loc modified by delta r dr and delta c dc."
  [loc dr &opt dc]
  [(+ (loc 0) dr) (+ (loc 1) (or dc 0))])

# from here on, a "box loc" is the loc of the :lbox

(defn boxes-touching
  "Return an array of the 0, 1, or 2 boxes touching this one in vertical
direction dr."
  [m loc dr]
  (def touching @[])
  (var toucher-loc (d-loc loc dr))
  (var toucher (mx/mget m ;toucher-loc))
  # one box sitting right above/below
  (if (= toucher :lbox)
    (array/push touching toucher-loc))
  # one box hanging out to the left)
  (if (= toucher :rbox)
    (array/push touching (d-loc toucher-loc 0 -1)))
  # one box hanging out to the right
  (var toucher-of-rbox-loc (d-loc loc dr 1))
  (set toucher (mx/mget m ;toucher-of-rbox-loc))
  (if (= toucher :lbox)
    (array/push touching toucher-of-rbox-loc))
  touching)

(defn find-pushable-boxes
  "Return an array of lbox locations that would be pushed if the box at loc
gets pushed with a row delta of dr. Includes the box at loc."
  [m loc dr]
  (def boxes @[loc])
  (var queue (boxes-touching m loc dr))
  (while (not (empty? queue))
    (let [box (array/pop queue)]
      (array/push boxes box)
      (set queue (distinct (array/push queue ;(boxes-touching m box dr))))))
  (distinct boxes))

(defn can-box-move?
  "Returns true if box with :lbox at loc can be moved with a row delta of
dr."
  [m loc dr]
  (and (not= :wall (mx/mget m ;(d-loc loc dr)))
       (not= :wall (mx/mget m ;(d-loc loc dr 1)))))

(defn simple-box-move
  "Moves box with lbox at loc in direction dr with no other checks."
  [m loc dr]
  (def [r c] loc)
  (mx/mput m ;(d-loc loc dr 0) :lbox)
  (mx/mput m ;(d-loc loc dr 1) :rbox)
  (mx/mput m ;(d-loc loc 0 0) nil)
  (mx/mput m ;(d-loc loc 0 1) nil))

(defn move-thing
  [m me loc move-offset]
  (def next-loc (d-loc loc ;move-offset))
  (def thing (mx/mget m ;next-loc))
  (cond
    # wall
    (= thing :wall)
    loc
    # emptiness; move there the next loc
    (nil? thing)
    (do
      (mx/mput m ;next-loc me)
      (mx/mput m ;loc nil)
      next-loc)
    # box, or lbox/rbox and we're moving horizontally
    (or (= thing :box)
        (and (or (= thing :lbox) (= thing :rbox))
             (zero? (move-offset 0))))
    (let [thing-next-loc (move-thing m thing next-loc move-offset)]
      (if (= thing-next-loc next-loc) # couldn't move it
        loc
        (do
          (mx/mput m ;next-loc me)
          (mx/mput m ;loc nil)
          next-loc)))
    # double-wide box and we're moving vertically
    (let [dr (if (> (next-loc 0) (loc 0)) 1 -1)
          pushable-box-locs (find-pushable-boxes
                             m
                             (if (= :lbox thing) next-loc (d-loc next-loc 0 -1))
                             dr)]
      (if (all |(can-box-move? m $ dr) pushable-box-locs)
        (do
          (sort-by (fn [loc] (* (loc 0) (- 0 dr)))
                   pushable-box-locs)
          (map |(simple-box-move m $ dr) pushable-box-locs)
          (mx/mput m ;next-loc me)
          (mx/mput m ;loc nil)
          next-loc)
        loc))))

(defn run-robot
  [m moves]
  (var robot-loc (mx/find-loc m :bot))
  (each move moves
    (set robot-loc (move-thing m :bot robot-loc (move-offsets move)))))

(defn gps-val [loc] (+ (* 100 (loc 0)) (loc 1)))

# ================ part 1 ================

(defn part1
  [lines]
  (def [m moves] (read-map-and-moves lines))
  (run-robot m moves)
  (+ ;(map gps-val (find-boxes m))))

# ================ part 2 ================

(defn embiggened-map
  [m]
  (seq [[r row] :pairs m]
     (def big-row @[])
     (loop [[c cell] :pairs row]
       (case cell
         :bot (array/push big-row :bot nil)
         :box (array/push big-row :lbox :rbox)
         (array/push big-row cell cell)))
     big-row))

(defn part2
  [lines]
  (var [m moves] (read-map-and-moves lines))
  (set m (embiggened-map m))
  (run-robot m moves)
  (+ ;(map gps-val (find-boxes m))))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 15))
