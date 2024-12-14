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
  "First time `func` is called with `args`, cache the result. Return that
cached value on subsequent calls.

Example

    (defn foo [x] (print \"calc x = \" x) (+ x 42))

    (pp (memoized foo 42))       # prints calc message, then value
    (pp (memoized foo (+ 2 40))) # value
    (pp (memoized foo (+ 3 39))) # prints value
    (pp (memoized foo 3))        # prints calc message then value"
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
  "Run `form`, print \"form => result\", and return the result."
  [form]
  (def result (gensym))
  ~(do
     (def ,result ,form)
     (printf "%j => %j" ',form ,result)
     ,result))
