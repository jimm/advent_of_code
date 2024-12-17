#!/usr/bin/env janet
#
# RAM Run

(import ../matrix :as mx)
(import ../running)
(import ../set)

# ================ helpers ================

(defn read-memory
  [lines size num-falling-bytes]
  (def m (mx/from-size size size :space))
  (loop [[i line] :pairs lines
         :while (< i num-falling-bytes)]
    (let [[r c] (map scan-number (string/split "," line))]
      (mx/mput m r c :wall)))
  m)

(defn path-neighbors
  "Return locations of horizontal/vertical path cells of [r c]."
  [maze r c]
  (filter |(= :space (mx/mget maze ;$))
          (map (fn [[dr dc]] [(+ r dr) (+ c dc)])
               [[0 1] [0 -1] [1 0] [-1 0]])))

(defn heuristic
  [maze p end]
  (def cell (mx/mget maze ;p))
  (if (= cell :wall)
    math/int-max
    (+ (- (end 0) (p 0))
       (- (end 1) (p 1)))))

(defn a-star
  [maze start end]
  (def came-from @{})
  (def oheap @[])
  (def gscore @{start 0})
  (def fscore @{start 0})
  (put fscore start (heuristic maze start end))
  (def close-set (set/new))
  (array/push oheap {:score (fscore start) :loc start})
  (def data @[])
  (while (and (not (empty? oheap)) (empty? data))
    #find oheap entry with min fscore
    (def min-fscore (min ;(map |($ :score) oheap)))
    (def i (find-index |(= min-fscore ($ :score)) oheap))
    (def fscore-and-cell (oheap i))
    (array/remove oheap i)
    (var current (fscore-and-cell :loc))

    (if (= current end)
      (do
        (while (has-key? came-from current)
          (array/push data current)
          (set current (came-from current))))
      (do
        (set/add close-set current)
        (each p (path-neighbors maze ;current)
          (def heur (heuristic maze p end))
          (def tentative-g-score (+ (gscore current) heur))

          (let [already-seen (close-set p)
                high-tentative-score (>= tentative-g-score (gscore p))
                p-already-in-oheap (find |(= $ p) (map |($ :loc) oheap))]
            (when (and (not (and already-seen high-tentative-score))
                       (or (not high-tentative-score)
                           p-already-in-oheap))
              (put came-from p current)
              (put gscore p tentative-g-score)
              (put fscore p (+ tentative-g-score heur))
              (array/push oheap {:score (fscore p) :loc p})))))))
  data)

# ================ part 1 ================

(defn part1
  [lines]
  (def size (if (dyn :testing) 7 71))
  (def num-falling-bytes (if (dyn :testing) 12 1024))
  (def m (read-memory lines size num-falling-bytes))
  (print)
  (mx/pp m {:wall "#"})
  (def path (a-star m [0 0] [(dec size) (dec size)]))
  (length path))

# ================ part 2 ================

(defn part2
  [lines]
  (def size (if (dyn :testing) 7 71))
  )

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 18))
