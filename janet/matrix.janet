# A two-dimensional mutable matrix. [0, 0] is at the top left of the matrix.
# Row increases down, column increases to the right.

(import ./util)

(defn from-size
  "Returns a rows x cols array of arrays filled with initial-value or nil."
  [rows cols &opt initial-value]
  (let [matrix (array/new rows)]
    (for i 0 cols
      (put matrix i (array/new-filled cols initial-value)))
    matrix))

(defn from-lines
  "Returns a matrix created from lines. Cells will contain byte values."
  [lines]
  (map (fn [line] (array ;(string/bytes line)))
       lines))

(defn height
  "Returns the number of rows in the matrix."
  [matrix]
  (length matrix))

(defn width
  "Returns the number of columns in the matrix."
  [matrix]
  (length (get matrix 0)))

(defn row
  "Returns row r of the matrix."
  [matrix r]
  (get matrix r))

(defn col
  "Returns column c of the matrix."
  [matrix c]
  (map (fn [r] (get r c)) matrix))

(defn mget
  "Returns cell (r, c) of the matrix or nil if out of bounds."
  [matrix r c]
  (util/dig matrix r c))

(defn mput
  "Sets cell (r, c) of the matrix to val."
  [matrix r c val]
  (put (get matrix r) c val))

(defn pprint
  [matrix]
  (map (fn [row] (print (string/from-bytes ;row))) matrix))
