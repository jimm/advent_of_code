(defvar *year* 2023)
(defvar *day* 1)

;; ================ part 1 ================

(defun first-and-last-digits (str)
  (let ((digits (loop for c across str  if (digit-char-p c) collect c)))
    (list (first digits) (car (last digits)))))

(defun first-and-last-digits-to-number (str)
  (let ((digit-chars (first-and-last-digits str)))
    (+ (* 10 (- (char-int (first digit-chars)) (char-int #\0)))
       (- (char-int (second digit-chars)) (char-int #\0)))))

;; ================ part 2 ================

;; ================ main ================

(defun part1 ()
  (loop for line in (input-lines 2023 1 1)
        summing
        (first-and-last-digits-to-number line)))

(defun test-part1 ()
  (let ((lines (input-lines 2023 1 1 :testing t)))
    ))

(defun part2 (&optional test-root)
  (let ((lines (input-lines 2023 1 2)))
    ))

(defun test-part2 ()
  (let ((lines (input-lines 2023 1 2 :testing t)))
    ))
