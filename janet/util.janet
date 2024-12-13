(defn words
  "Return the words in line, consolidating multiple consecutive separators.
The default separator is a space."
  [line &opt sep]
  (filter (complement empty?)
          (string/split (or sep " ") line)))

(defn extract-nums
  "Return the numbers in a line, skipping multiple consecutive spaces."
  [line &opt sep]
  (map scan-number (words line sep)))

(defn dig
  "Find and return a value in a nested data structure `ds` specified by
`keys`, or nil if any key is not found. If `keys` is empty, returns .

(def d {:a 1 :b {:c 42}}
(dig d :b :c)    # => 42
(dig d :x :c)    # => nil
(dig d)          # => {:a 1 :b {:c 42}}"
  [ds & keys]
  (var result ds)
  (loop [key :in keys
         :while result]
    (set result (get result key)))
  result)
    
(defmacro ord
  "Convert a byte to a one-character string."
  [s]
  ~(string/from-bytes ,s))

(defn in?
  "Return `val` if it is in `ind`, else nil.

In tables, this looks in values. To see if a dict has a key, use has-key?."
  [ind val]
  # for my own edification: equivalent to
  # (find (partial = val) ind)
  (find |(= val $) ind))

(defn memoized
  [func & args]
  (unless (dyn :memoize-cache)  (setdyn :memoize-cache @{}))
  (let [key [func ;args]
        cached ((dyn :memoize-cache) key)]
    (if cached
      # return value, decoding :memoize-nil if necessary
      (if (= cached :memoize-nil) nil cached)
      # cache value, encoding :memoize-nil if necessary, and return value
      (let [val (func ;args)]
        (put (dyn :memoize-cache) key (if (nil? val) :memoize-nil val))
        val))))

(defmacro inspect
  [form]
  (def result (gensym))
  ~(do
     (def ,result ,form)
     (printf "%j => %j" (quote ,form) ,result)
     ,result))
