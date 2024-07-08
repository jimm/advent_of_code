#!/usr/bin/env janet

# ================ Trebuchet?! ================

(import ../data :as data)
(import ../testing :as testing)
(import ../running :as running)

# ================ part 1 ================

(defn digit? [val]
  (and (>= val 48) (<= val 57)))

(defn ascii-to-int [val]
  (- val 48))

(defn first-and-last-digits [str]
  (let [digits (filter digit? (string/bytes str))]
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

(def digit-names ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])

(defn do-replace-words-with-digits [input output]
  (if (zero? (length input))
    output
    (do
      (var new-input nil)
      (var new-output nil)
      (for i 0 10
        (let [dname (digit-names i)]
          (when (and (not new-input)
                     (string/has-prefix? dname input))
            (do
              (set new-input (drop (length dname) input))
              (set new-output (string output i))))))
      (if new-input
        (do-replace-words-with-digits new-input new-output)
        (do-replace-words-with-digits (drop 1 input) (string output (take 1 input)))))))

(defn replace-words-with-digits [line]
  (do-replace-words-with-digits line ""))

(defn do-part2 [lines]
  (var total 0)
  (each line lines
    (+= total (first-and-last-digits-to-number (replace-words-with-digits line))))
  total)

# ================ main ================

(defn part1 []
  (do-part1 (data/input-lines 2023 1 1)))

(defn test-part1 []
  (testing/run-tests do-part1 2023 1 1))

(defn part2 []
  (do-part2 (data/input-lines 2023 1 2)))

(defn test-part2 []
  (testing/run-tests do-part2 2023 1 2))

(defn main [&]
  (running/run part1 test-part1 part2 test-part2))
