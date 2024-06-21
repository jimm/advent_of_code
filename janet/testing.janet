# ================ run tests ================

(import ./data :as data)

(defn run-test
  [func chunk part]
  "Run func and return true if matches the expected value from the first line."
  (let [first-line (string/slice (first chunk) 2) # skip leading "# " or "; "
        expected-list (string/split "," first-line)
        expected (if (and (= part 2) (>= (length expected-list) 2))
                   (expected-list  1)
                   (first expected-list))
        input (slice chunk 1)]
    # try converting expected value into integer, but it's OK if we can't
    (= expected (string (func input)))))

(defn -find-chunks
  [lines]
  "Returns a list of lists of lines."
  (filter |(not (= "" (first $)))
          (partition-by |(= $ "") lines)))

(defn run-tests
  [func year day part]
  (var test-count 0)
  (var pass-count 0)
  (each chunk (-find-chunks (data/input-lines year day part :testing true))
    (let [ok-p (run-test func chunk part)]
      (+= test-count 1)
      (if ok-p
        (do
          (+= pass-count 1)
          (prin "."))
        (prin "F")))
    (print "")
    (print " Tests: " test-count)
    (print "Passed: " pass-count)
    (print "Failed: " (- test-count pass-count))))
