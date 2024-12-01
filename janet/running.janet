(import ./data)
(import ./testing)

# ================ running ================

(defn run [p1 tp1 p2 tp2]
  (def testing (= ((dyn :args) 1) "-t"))
  (def part (parse ((dyn :args) (if testing 2 1))))
  (when (= part 1)
    (if testing (tp1) (print (p1))))
  (when (= part 2)
    (if testing (tp2) (print (p2)))))

(defn run-main
  "Defines part{1,2}, test-part{1,2}, and main functions."
  [do-part1 do-part2 year day]
  (defn part1 []
    (do-part1 (data/input-lines year day 1)))

  (defn test-part1 []
    (testing/run-tests do-part1 year day 1))

  (defn part2 []
    (do-part2 (data/input-lines year day 2)))

  (defn test-part2 []
    (testing/run-tests do-part2 year day 2))

  (run part1 test-part1 part2 test-part2))
