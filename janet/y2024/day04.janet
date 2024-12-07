#!/usr/bin/env janet
#
# Ceres Search

(import ../matrix :as mx)
(import ../running)

# ================ helpers ================

(def dir-offsets [[-1 0] [-1 1] [0 1] [1 1]
                  [1 0] [1 -1] [0 -1] [-1 -1]])
(def bx (chr "X"))
(def bm (chr "M"))
(def ba (chr "A"))
(def bs (chr "S"))

(defn xmas-in-dir-at?
  [m x-loc dir-offset]
  (def [r c] x-loc)
  (def [dr1 dc1] dir-offset)
  (def [dr2 dc2] [(* 2 dr1) (* 2 dc1)])
  (def [dr3 dc3] [(* 3 dr1) (* 3 dc1)])
  (and (= bm (mx/mget m (+ r dr1) (+ c dc1)))
       (= ba (mx/mget m (+ r dr2) (+ c dc2)))
       (= bs (mx/mget m (+ r dr3) (+ c dc3)))))

(defn count-xmas-at
  [m x-loc]
  (length (filter (fn [dir-offset] (xmas-in-dir-at? m x-loc dir-offset))
                  dir-offsets)))

(defn count-all-xmas
  [m]
  (var x-locs @[])
  (loop [r :range [0 (mx/height m)]
         c :range [0 (mx/width m)]]
    (when (= bx (mx/mget m r c))
      (array/push x-locs [r c])))
  (+ ;(map (fn [x-loc] (count-xmas-at m x-loc)) x-locs)))

(defn x-mas?
  [m a-loc]
  (def [r c] a-loc)
  (def topleft [(- r 1) (- c 1)])
  (def topright [(- r 1) (+ c 1)])
  (def botleft [(+ r 1) (- c 1)])
  (def botright [(+ r 1) (+ c 1)])
  (and
    (or
      (and (= bm (mx/mget m ;topleft))
           (= bs (mx/mget m ;botright)))
      (and (= bs (mx/mget m ;topleft))
           (= bm (mx/mget m ;botright))))
    (or
      (and (= bm (mx/mget m ;topright))
           (= bs (mx/mget m ;botleft)))
      (and (= bs (mx/mget m ;topright))
           (= bm (mx/mget m ;botleft))))))

(defn count-all-x-mas
  [m]
  (var a-locs @[])
  (loop [r :range [0 (mx/height m)]
         c :range [0 (mx/width m)]]
    (when (= ba (mx/mget m r c))
      (array/push a-locs [r c])))
  (length (filter (fn [a-loc] (x-mas? m a-loc)) a-locs)))

# ================ part 1 ================

(defn part1 [lines]
  (def m (mx/from-lines lines))
  (count-all-xmas m))

# ================ part 2 ================

(defn part2 [lines]
  (def m (mx/from-lines lines))
  (count-all-x-mas m))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 4))
