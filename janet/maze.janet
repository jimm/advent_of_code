(import ./matrix :as mx)

(def path (chr "."))
(def wall (chr "#"))
(def start (chr "S"))
(def end (chr "E"))

(defn from-lines [lines] (mx/from-lines lines))

(defn start-loc [maze] (mx/find-loc maze start))

(defn end-loc [maze] (mx/find-loc maze end))

(defn path-neighbors
  "Return locations of horizontal/vertical path cells of [r c]."
  [maze r c]
  (filter |(not= wall (mx/mget maze ;$))
          (map (fn [[dr dc]] [(+ r dr) (+ c dc)])
               [[0 1] [0 -1] [1 0] [-1 0]])))

(defn dead-end?
  "Return true if [r c] is a dead end."
  [maze r c]
  (and (= path (mx/mget maze r c))
       (= 1 (length (path-neighbors maze r c)))))

(defn find-dead-ends
  "Return the locations of all dead ends in a maze."
  [maze]
  (def dead-ends @[])
  (loop [[r row] :pairs maze
         [c cell] :pairs row
         :unless (= cell wall)
         :when (dead-end? maze r c)]
    (array/push dead-ends [r c]))
  dead-ends)

(defn dead-end-path-next
  "Given a dead end location, return the open path location next to it that
  is also a part of the dead end. Return nil otherwise."
  [maze r c]
  (def next-path-loc (first (path-neighbors maze r c)))
  (when (and next-path-loc (dead-end? maze ;next-path-loc))
    next-path-loc))

(defn fill-dead-end
  "Given a dead end, destructively fill the dead end path with `wall`."
  [maze r c]
  (mx/mput maze r c wall)
  (var next-path-loc (dead-end-path-next maze r c))
  (while (and next-path-loc (= (mx/mget maze ;next-path-loc) path))
    (mx/mput maze ;next-path-loc wall)
    (set next-path-loc (dead-end-path-next maze ;next-path-loc))))

(defn remove-dead-ends
  "Destructively fill maze's dead end paths with `wall`. Return maze."
  [maze]
  (each dead-end (find-dead-ends maze)
    (when (= path (mx/mget maze ;dead-end)) # not start or end
      (fill-dead-end maze ;dead-end)))
  maze)

(defn paths-from
  "Return an array of all paths from `loc` to `end`."
  [maze loc end curr-path paths]
  # optimization: neighbors will be close to the end
  (def path-end (if (> 2 (length curr-path)) (slice curr-path -3) curr-path))
  (loop [p :in (path-neighbors maze ;loc)
         :when (not (find |(= p $) path-end))]
    (if (= p end)
        (array/push paths (array ;curr-path p))
      (do
        (array/push curr-path p)
        (paths-from maze p end curr-path paths)
        (array/pop curr-path))))
  paths)

(defn find-all-paths
  [maze &opt a b]
  "Return an array of all paths from a (default start) to b (default end)."
  (def start (or a (mx/find-loc maze start)))
  (def end (or b (mx/find-loc maze end)))
  (def maze-no-dead-ends (remove-dead-ends (mx/copy maze)))
  (paths-from maze-no-dead-ends start end @[start] @[]))
