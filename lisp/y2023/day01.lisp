(defvar *year* 2023)
(defvar *day* 1)

;; ================ part 1 ================

(defun -part1 (lines)
  (loop for line in lines
        summing
        (first-and-last-digits-to-number line)))

(defun first-and-last-digits (str)
  (let ((digits (loop for c across str  if (digit-char-p c) collect c)))
    (list (first digits) (car (last digits)))))

(defun first-and-last-digits-to-number (str)
  (let ((digit-chars (first-and-last-digits str)))
    (+ (* 10 (- (char-int (first digit-chars)) (char-int #\0)))
       (- (char-int (second digit-chars)) (char-int #\0)))))

;; ================ part 2 ================

(defun -part2 (lines)
  )

;; ================ main ================

(defun part1 ()
  (-part1 (input-lines 2023 1 1)))

(defun test-part1 ()
  (run-tests #'-part1 2023 1 1))

(defun part2 ()
  (-part2 (input-lines 2023 1 2)))

(defun test-part2 ()
  (run-tests #'-part2 2023 1 2))
