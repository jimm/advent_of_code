(defn words
  "Returns words in line, skipping multiple consecutive spaces.

Can't just call string/split because it returns empty strings if there are
multiple consecutive spaces."
  [line]
  (filter (fn [s] (> (length s) 0))
          (string/split " " line)))
