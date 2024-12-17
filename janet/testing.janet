# ================ run tests ================

(import ./data :as data)

(defn- parse-expected-line
  "If line starts with \"#j \" or \";j \" eval the remainder of the line,
  which should be a collection. Else split the line at commas."
  [line]
  (if (= (line 1) (chr "j"))
    (eval-string (slice line 3))
    (string/split "," (slice line 2))))

(defn run-test
  "Run func and return [expected result]."
  [func chunk part]
  (let [expected-list (parse-expected-line (first chunk))
        expected (if (and (= part 2) (>= (length expected-list) 2))
                   (expected-list  1)
                   (first expected-list))
        input (slice chunk 1)]
    (setdyn :testing-expected-value expected)
    [expected (string (func input))]))

(defn find-chunks
  "Returns a list of lists of lines used as a test."
  [lines]
  (def expected-char (slice (first lines) 0 1))
  (var expecteds-and-datas
    (partition-by |(and (not (empty? $))
                        (= (slice $ 0 1) expected-char))
                  lines))
  # remove trailing empty lines
  (set expecteds-and-datas (map |(if (empty? (last $)) (slice $ 0 -2) $)
                                expecteds-and-datas))
  (map (fn [[expected-lines data-lines]]
         (array (first expected-lines) ;data-lines))
       (partition 2 expecteds-and-datas)))

(defn run-tests
  [func year day part &opt keep-blank-lines]
  (var test-count 0)
  (var fail-messages @[])
  (setdyn :testing true)
  (each chunk (find-chunks (data/input-lines
                             year day part
                             :testing true
                             :keep-blank-lines keep-blank-lines))
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
  (setdyn :testing false)
  (print "")
  (print " Tests: " test-count)
  (print "Passed: " (- test-count (length fail-messages)))
  (print "Failed: " (length fail-messages))
  (print (string/join fail-messages "\n")))
