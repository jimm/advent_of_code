# ================ data input ================

(defn input-lines-from
  [path &keys {:keep-blank-lines keep-blank-lines}]
  "Reads a file and returns a list containing the lines in the file, without
newlines. Does not return blank lines unless `:keep-blank-lines` is true."
  (let [lines (string/split "\n" (slurp path))]
    (if keep-blank-lines
      lines
      (filter |(not (= "" $)) lines))))

(defn data-file-path
  [year day part testing]
  "Returns a path to the data file for `year`, `day`, and `part` (may be nil).
If `testing` is non-nil, adds \"_test\" before the file extension."
  (var fname (string/format "day%02d" day))
  (when part (set fname (string fname "_" part)))
  (when testing (set fname (string fname "_test")))
  (string "../../data/y" year "/" fname ".txt"))

(defn input-lines
  [year day part &keys {:testing testing :keep-blank-lines keep-blank-lines}]
  "INPUT-LINES calls INPUT-LINES-FROM with the path to the data file for
`year`, `day`, `part`, and optionally \"_test\" before the file extension.
Keeps blank lines if `keep-blank-lines` is non-nil.

If the file is not found and `part` > `, try with `part` = 1. If that is not
found, try it without a part number at all."
  (var path (data-file-path year day part testing))
  (var file (file/open path))
  (when (and (not file) (not (= part 1)))
    (set path (data-file-path year day 1 testing))
    (set file (file/open path)))
  (when (not file)
    (set path (data-file-path year day nil testing))
    (set file (file/open path)))
  (if file
    (do
      (file/close file)
      (input-lines-from path :keep-blank-lines keep-blank-lines))
    (error (string "can't find data file matching year " year ", day " day ", part " part ", testing " (truthy? testing)))))

