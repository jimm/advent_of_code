# ================ Trebuchet?! ================

(import ../data :as data)
(import ../testing :as testing)

# ================ part 1 ================

(defn digitp [val]
  (and (>= val 48) (<= val 57)))

(defn ascii-to-int [val]
  (- val 48))

(defn first-and-last-digits [str]
  (let [digits (filter digitp (string/bytes str))]
    [(ascii-to-int (first digits)) (ascii-to-int (first (reverse digits)))]))

(defn first-and-last-digits-to-number [str]
  (let [digits (first-and-last-digits str)]
    (+ (* 10 (digits 0)) (digits 1))))

(defn do-part1 [lines]
  (var total 0)
  (each line lines
    (+= total (first-and-last-digits-to-number line)))
  total)

# ================ part 2 ================

(defn do-part2 [lines]
  )

# ================ main ================

(defn part1 []
  (do-part1 (data/input-lines 2023 1 1)))

(defn test-part1 []
  (testing/run-tests do-part1 2023 1 1))

(defn part2 []
  (do-part2 (data/input-lines 2023 1 2)))

(defn test-part2 []
  (testing/run-tests do-part2 2023 1 2))
