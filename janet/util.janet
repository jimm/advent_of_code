(import spork/misc)

(defn words
  "Returns words in line, consolidating multiple consecutive separators.
Default separator is a space."
  [line &opt sep]
  (filter (complement empty?)
          (string/split (or sep " ") line)))

(defn dig
  "Finds and returns obj in nested objs specified by keys."
  [obj & keys]
  (var result obj)
  (loop [key :in keys
         :while result]
    (set result (get result key)))
  result)
    
(defmacro ord
  "Converts a byte to a one-character string."
  [s]
  ~(string/from-bytes ,s))

# see also scan-number which handles floats.
(defn stoi
  [s &opt base]
  (misc/string->int s (or base 10)))

(defn extract-nums
  "Returns numbers in a line, skipping multiple consecutive spaces."
  [line]
  (map stoi (words line)))

(defn includes?
  [xs val]
  # for my own edification: equivalent to
  # (find (partial = val) xs)
  (find |(= val $) xs))
