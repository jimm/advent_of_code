# ================ run tests ================

(import ./data :as data)

(defn run-test
  "Run func and return [expected result]."
  [func chunk part]
  (let [first-line (string/slice (first chunk) 2) # skip leading "# " or "; "
        expected-list (string/split "," first-line)
        expected (if (and (= part 2) (>= (length expected-list) 2))
                   (expected-list  1)
                   (first expected-list))
        input (slice chunk 1)]
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
