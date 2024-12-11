(import spork/argparse)
(import spork/path)
(import ./data)
(import ./testing)

# ================ running ================

(defn run [p1 tp1 p2 tp2]
  (def opts
    (argparse/argparse
     "Run an AoC solution."
     "test" {:short "t"
             :kind :flag
             :help "Run tests"}
     "timer" {:short "T"
              :kind :flag
                :help "Output execution time"}
     :default {:kind :option
               :map scan-number}))
  (def start-time (when (opts "timer") (os/clock :monotonic)))

  (let [part (opts :default)
        testing (opts "test")]
    (cond (= part 1)
          (if testing (tp1) (print (p1)))
          (= part 2)
          (if testing (tp2) (print (p2)))
          true
          (errorf "Only part numbers 1 and 2 make sense.")))

  (when (opts "timer")
    (def now (os/clock :monotonic))
    (printf "Runtime: %0.3f seconds" (- now start-time))))

(defn run-main
  "Defines part{1,2}, test-part{1,2}, and main functions."
  [do-part1 do-part2 year day &opt keep-blank-lines]
  (defn part1 []
    (do-part1 (data/input-lines year day 1 :keep-blank-lines keep-blank-lines)))

  (defn test-part1 []
    (testing/run-tests do-part1 year day 1 keep-blank-lines))

  (defn part2 []
    (do-part2 (data/input-lines year day 2 :keep-blank-lines keep-blank-lines)))

  (defn test-part2 []
    (testing/run-tests do-part2 year day 2 keep-blank-lines))

  (run part1 test-part1 part2 test-part2))
