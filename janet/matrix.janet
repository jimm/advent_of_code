# A two-dimensional mutable matrix. [0, 0] is at the top left of the matrix.
# Row increases down, column increases to the right.

(import ./util)

(defn from-size
  "Return a rows x cols array of arrays filled with initial-value or nil."
  [rows cols &opt initial-value]
  (let [m (array/new rows)]
    (for i 0 cols
      (put m i (array/new-filled cols initial-value)))
    m))

(defn from-lines
  "Return a matrix created from lines. Cells will contain byte values."
  [lines]
  (map (fn [line] (array ;(string/bytes line)))
       lines))

(defn height
  "Return the number of rows in the matrix."
  [m]
  (length m))

(defn width
  "Return the number of columns in the matrix."
  [m]
  (length (get m 0)))

(defn in-bounds?
  [m r c]
  (and (>= r 0)
       (>= c 0)
       (< r (height m))
       (< c (width m))))

(defn row
  "Return row r of the matrix."
  [m r]
  (get m r))

(defn col
  "Return column c of the matrix."
  [m c]
  (map (fn [r] (get r c)) m))

(defn mget
  "Return cell (r, c) of the matrix or nil if out of bounds."
  [m r c]
  (util/dig m r c))

(defn mput
  "Set cell (r, c) of the matrix to val. Does nothing if [r c] is out of
bounds."
  [m r c val]
  (when (in-bounds? m r c)
    (put (get m r) c val)))

(defn find-loc
  "Return the [r, c] loc of the first cell that satisfies a predicate or
equals a value, or nil if not found. Search is done row by row, col by col."
  [m pred-or-val]
  (def pred (if (function? pred-or-val) pred-or-val |(= $ pred-or-val)))
  (var loc nil)
  (loop [[r row] :pairs m
         :while (not loc)]
    (if-let [c (find-index pred row)]
      (set loc [r c])))
  loc)

(defn find-locs
  "Return an array containing the [r, c] locs of all cells that satisfy a
predicate or equal a value, or an empty array if not found. Search is done
row by row, col by col."
  [m pred-or-val]
  (def pred (if (function? pred-or-val) pred-or-val |(= $ pred-or-val)))
  (var locs @[])
  (loop [[r row] :pairs m
         [c cell] :pairs row]
    (when (pred cell)
      (array/push locs [r c])))
  locs)

(defn copy
  "Return a copy of m."
  [m]
  (map |(array ;(slice $)) m))

(defn pp
  "Pretty-print the matrix. Assumes values are bytes and prints the rows as
strings."
  [m]
  (map (fn [row] (print (string/from-bytes ;row))) m))
