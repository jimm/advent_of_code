# ================ PUZZLENAME ================

(import ../data :as data)
(import ../testing :as testing)

# ================ part 1 ================

(defn do-part1 [lines]
  )

# ================ part 2 ================

(defn do-part2 [lines]
  )

# ================ main ================

(defn part1 []
  (do-part1 (data/input-lines YEAR DAY 1)))

(defn test-part1 []
  (testing/run-tests do-part1 YEAR DAY 1))

(defn part2 []
  (do-part2 (data/input-lines YEAR DAY 2)))

(defn test-part2 []
  (testing/run-tests do-part2 YEAR DAY 2))
