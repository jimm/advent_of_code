(defn words
  "Returns words in line, consolidating multiple consecutive separators.
Default separator is a space."
  [line &opt sep]
  (filter (complement empty?)
          (string/split (or sep " ") line)))

(defn dig
  "Finds and returns value in nested data structure ds specified by keys, or
nil if any key is not found. If keys is empty, returns obj.

(def d {:a 1 :b {:c 42}}
(dig d :b :c)    # => 42
(dig d :x :c)    # => nil"
  [ds & keys]
  (var result ds)
  (loop [key :in keys
         :while result]
    (set result (get result key)))
  result)
    
(defmacro ord
  "Converts a byte to a one-character string."
  [s]
  ~(string/from-bytes ,s))

(defn extract-nums
  "Returns numbers in a line, skipping multiple consecutive spaces."
  [line &opt sep]
  (map scan-number (words line sep)))

(defn in?
  "Returns val if it is in ind, else nil."
  [ind val]
  # for my own edification: equivalent to
  # (find (partial = val) ind)
  (find |(= val $) ind))
