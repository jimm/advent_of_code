;;; ================ run tests ================

(defun run-test (func chunk part)
  "Run func and return t if matches the expected value from the first line."
  (let* ((first-line (subseq (car chunk) 2)) ; skip leading "# " or "; "
         (expected (nth (1- part) (split-sequence:split-sequence #\, first-line)))
         (input (cdr chunk)))
    ;; try converting expected value into integer, but it's OK if we can't
    (equal (or (parse-integer expected :junk-allowed t)
               expected)
           (funcall func input))))

(defun -find-chunks (lines)
  "Returns a list of lists of lines."
  (split-sequence:split-sequence (lambda (line) (zerop (length line))) lines))

(defun run-tests (func year day part)
  (loop for chunk in (-find-chunks (input-lines year day part :testing t))
        with test-count = 0
        with pass-count = 0
        do
        (let ((ok-p (run-test func chunk part)))
          (setq test-count (1+ test-count))
          (if ok-p
              (progn
                (setq pass-count (1+ pass-count))
                (princ #\.))
              (princ #\F)))
        finally
        (progn
          (format t "~%")
          (format t " Tests: ~a~%" test-count)
          (format t "Passed: ~a~%" pass-count)
          (format t "Failed: ~a~%" (- test-count pass-count)))))
