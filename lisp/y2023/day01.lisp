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

(defun do-part1 (lines)
  (loop for line in lines
        summing
        (first-and-last-digits-to-number line)))

;; ================ part 2 ================

(defconstant digit-names '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(defun do-replace-words-with-digits (input output)
  (if (zerop (length input))
      output
      (let ((changed nil))
        (loop for dname in digit-names
              with n = 0
              when (and (not changed) (starts-with-p input dname))
              return
                 (setq input (drop input (length dname))
                       output (concat output (write-to-string n))
                       changed t)
              do
                 (setq n (1+ n)))
        (if changed
            (do-replace-words-with-digits input output)
            (do-replace-words-with-digits (drop input 1) (concat output (take input 1)))))))

(defun replace-words-with-digits (line)
  (do-replace-words-with-digits line ""))

(defun do-part2 (lines)
  (loop for line in lines
        summing
        (first-and-last-digits-to-number (replace-words-with-digits line))))

;; ================ main ================

(defun part1 ()
  (do-part1 (input-lines 2023 1 1)))

(defun test-part1 ()
  (run-tests #'do-part1 2023 1 1))

(defun part2 ()
  (do-part2 (input-lines 2023 1 2)))

(defun test-part2 ()
  (run-tests #'do-part2 2023 1 2))
