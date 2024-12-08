# A two-dimensional mutable matrix. [0, 0] is at the top left of the matrix.
# Row increases down, column increases to the right.

(import ./util)

(defn from-size
  "Returns a rows x cols array of arrays filled with initial-value or nil."
  [rows cols &opt initial-value]
  (let [m (array/new rows)]
    (for i 0 cols
      (put m i (array/new-filled cols initial-value)))
    m))

(defn from-lines
  "Returns a matrix created from lines. Cells will contain byte values."
  [lines]
  (map (fn [line] (array ;(string/bytes line)))
       lines))

(defn height
  "Returns the number of rows in the matrix."
  [m]
  (length m))

(defn width
  "Returns the number of columns in the matrix."
  [m]
  (length (get m 0)))

(defn in-bounds?
  [m r c]
  (and (>= r 0)
       (>= c 0)
       (< r (height m))
       (< c (width m))))

(defn row
  "Returns row r of the matrix."
  [m r]
  (get m r))

(defn col
  "Returns column c of the matrix."
  [m c]
  (map (fn [r] (get r c)) m))

(defn mget
  "Returns cell (r, c) of the matrix or nil if out of bounds."
  [m r c]
  (util/dig m r c))

(defn mput
  "Sets cell (r, c) of the matrix to val. Does nothing if [r c] is out of
bounds."
  [m r c val]
  (if (in-bounds? m r c)
    (put (get m r) c val)
    (print (string/format "error: [%d %d] is out of bounds" r c))))

(defn find
  "Returns the [r, c] loc of the first occurrence of val or nil if not
found. Search is done row by row, col by col."
  [m val]
  (var loc nil)
  (loop [r :range [0 (height m)]
         :while (not loc)]
    (def row (m r))
    (loop [c :range [0 (width m)]
           :while (not loc)]
      (when (= (row c) val)
        (set loc [r c]))))
  loc)

(defn copy
  [m]
  (map |(array ;(slice $)) m))

(defn pprint
  [m]
  (map (fn [row] (print (string/from-bytes ;row))) m))
