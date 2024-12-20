(import ./matrix :as mx)

(def path (chr "."))
(def wall (chr "#"))
(def start (chr "S"))
(def end (chr "E"))

(defn from-lines [lines] (mx/from-lines lines))

(defn path-neighbors
  "Return locations of horizontal/vertical path cells of [r c]."
  [maze r c]
  (filter |(not= wall (mx/mget maze ;$))
          (map (fn [[dr dc]] [(+ r dr) (+ c dc)])
               [[0 1] [0 -1] [1 0] [-1 0]])))

(defn dead-end?
  [maze r c]
  (and (= path (mx/mget maze r c))
       (= 1 (length (path-neighbors maze r c)))))

(defn find-dead-ends
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
  [maze r c]
  (mx/mput maze r c wall)
  (var next-path-loc (dead-end-path-next maze r c))
  (while (and next-path-loc (= (mx/mget maze ;next-path-loc) path))
    (mx/mput maze ;next-path-loc wall)
    (set next-path-loc (dead-end-path-next maze ;next-path-loc))))

(defn remove-dead-ends
  [maze]
  (each dead-end (find-dead-ends maze)
    (when (= path (mx/mget maze ;dead-end)) # not start or end
      (fill-dead-end maze ;dead-end))))

(defn paths-from
  [maze loc end curr-path paths]
  (loop [p :in (path-neighbors maze ;loc)
         :when (not (find |(= p $) curr-path))]
    (if (= p end)
      (array/push paths (array ;curr-path p))
      (do
        (array/push curr-path p)
        (paths-from maze p end curr-path paths)
        (array/pop curr-path))))
  paths)

(defn find-all-paths
  [maze]
  (def start (mx/find-loc maze start))
  (def end (mx/find-loc maze end))
  (paths-from maze start end @[start] @[]))
