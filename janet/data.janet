(import spork/path)
(import spork/sh)

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
  (string (path/dirname (dyn *current-file*))
          "../../data/y" year "/" fname ".txt"))

(defn input-lines
  [year day part &keys {:testing testing :keep-blank-lines keep-blank-lines}]
  "Returns an array of lines from the data file for year, day, and the testing
flag. Keeps blank lines if `keep-blank-lines` is non-nil.

If the file is not found and `part` > 1, try with `part` = 1. If that is not
found, try it without a part number at all. Prints a message and rases an error
if no file is found."
  (var path (data-file-path year day part testing))
  # if not found and part > 1, look for part 1
  (when (and (not (sh/exists? path)) (not (= part 1)))
    (set path (data-file-path year day 1 testing)))
  # if not found, look for file with no part number
  (when (not (sh/exists? path))
    (set path (data-file-path year day nil testing)))
  # if not found, print a message and raise an error
  (when (not (sh/exists? path))
    (errorf "can't find %s file for %d day %d part %d"
            (if (truthy? testing) "test" "data") year day part))
  (input-lines-from path :keep-blank-lines keep-blank-lines))

