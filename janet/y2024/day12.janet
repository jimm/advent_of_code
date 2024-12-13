#!/usr/bin/env janet
#
# Garden Groups

(import ../matrix :as mx)
(import ../running)
(import ../util)

# ================ helpers ================

(defn abuts?
  [[ar ac] [br bc]]
  (def dr (- ar br))
  (def dc (- ac bc))
  (or (and (zero? dr) (= 1 (math/abs dc)))
      (and (zero? dc) (= 1 (math/abs dr)))))

(defn flood-find
  "Gather all locs in the same region as loc and return them, setting their values in m to 0."
  [m loc]
  (def val (mx/mget m ;loc))
  (var queue @[loc])
  (var locs @[])
  (while (not (empty? queue))
    (let [[r c] (array/pop queue)]
      (when (= val (mx/mget m r c))
        (mx/mput m r c 0)
        (array/push locs [r c])
        (array/push queue [(inc r) c] [(dec r) c] [r (inc c)] [r (dec c)]))))
  locs)

(defn find-regions
  "Return all regions in m."
  [m]
  (def regions @[])
  (var loc (mx/find-loc m (complement zero?)))
  (while loc
    (array/push regions (flood-find m loc))
    (set loc (mx/find-loc m |(not (zero? $)))))
  regions)

# ================ part 1 ================
    
(defn fence-price-length
  [region]
  (var perimeter 0)
  (each [r c] region
    (+= perimeter
        (if (util/in? region [(inc r) c]) 0 1)
        (if (util/in? region [(dec r) c]) 0 1)
        (if (util/in? region [r (inc c)]) 0 1)
        (if (util/in? region [r (dec c)]) 0 1)))
  (* (length region) perimeter))

(defn part1 [lines]
  (def m (mx/from-lines lines))
  (def regions (find-regions m))
  (+ ;(map fence-price-length regions)))

# ================ part 2 ================
    
(defn inner-corner-count
  [region [r c]]
  (def neighbor (partial (comp util/in?) region))
  (def no-neighbor (complement neighbor))
  (def n (neighbor [(dec r) c]))
  (def s (neighbor [(inc r) c]))
  (def e (neighbor [r (inc c)]))
  (def w (neighbor [r (dec c)]))
  (def no-ne (no-neighbor [(dec r) (inc c)]))
  (def no-se (no-neighbor [(inc r) (inc c)]))
  (def no-sw (no-neighbor [(inc r) (dec c)]))
  (def no-nw (no-neighbor [(dec r) (dec c)]))
  (length (filter identity [(and no-ne n e)
                            (and no-se s e)
                            (and no-sw s w)
                            (and no-nw n w)])))

(defn outer-corner-count
  [region [r c]]
  (def no-neighbor (partial (comp not util/in?) region))
  (def no-n (no-neighbor [(dec r) c]))
  (def no-s (no-neighbor [(inc r) c]))
  (def no-e (no-neighbor [r (inc c)]))
  (def no-w (no-neighbor [r (dec c)]))
  (length (filter identity [(and no-n no-w)
                            (and no-n no-e)
                            (and no-s no-w)
                            (and no-s no-e)])))

(defn corner-count
  "Return the number of corners at this loc."
  [region loc]
  (+ (outer-corner-count region loc)
     (inner-corner-count region loc)))

(defn fence-price-sides
  [region]
  (def num-sides (+ ;(map (partial corner-count region) region)))
  (* (length region) num-sides))

(defn part2 [lines]
  (def m (mx/from-lines lines))
  (def regions (find-regions m))
  (+ ;(map fence-price-sides regions)))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 12))
