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
    [expected (string (func input))]))
    # (= expected (string (func input)))))

(defn -find-chunks
  [lines]
  "Returns a list of lists of lines."
  (filter |(not (= "" (first $)))
          (partition-by |(= $ "") lines)))

(defn run-tests
  [func year day part]
  (var test-count 0)
  (var fail-messages @[])
  (each chunk (-find-chunks (data/input-lines year day part :testing true))
    (do
      (def [expected result] (run-test func chunk part))
      (+= test-count 1)
      (if (= expected result)
        (do
          (prin "."))
        (do
          (prin "F")
          (array/push fail-messages
                      (string "Test " test-count " failed: expected " expected ", got " result))))))
  (print "")
  (print " Tests: " test-count)
  (print "Passed: " (- test-count (length fail-messages)))
  (print "Failed: " (length fail-messages))
  (print (string/join fail-messages "\n")))
