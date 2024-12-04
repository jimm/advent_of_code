(import spork/misc)

(defn words
  "Returns words in line, skipping multiple consecutive spaces.

Can't just call string/split because it returns empty strings if there are
multiple consecutive spaces."
  [line]
  (filter (fn [s] (> (length s) 0))
          (string/split " " line)))

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
  (string/from-bytes s))

(defn stoi
  [s]
  (misc/string->int s))

(defn extract-nums [line]
  "Returns numbers in a line, skipping multiple consecutive spaces."
  (map (fn [s] (stoi s)) (words line)))
