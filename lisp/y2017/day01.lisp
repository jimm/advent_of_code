;;; Inverse Captcha

(defun digits (n)
  (map 'list #'digit-char-p (prin1-to-string n)))

(defun puzzle-digits ()
  (digits (to-integer (car (input-lines 2017 1)))))

;;; ================ part 1 ================

(defun sum-next-matchers (digits)
  (loop for digit in digits
     with prev-digit
     when (eq prev-digit digit)
       sum digit into summed
     end
     do (setf prev-digit digit)
     finally (return summed)))

(defun part1 ()
  (let* ((digits (puzzle-digits))
         (wrapped (append digits (list (car digits)))))
    (sum-next-matchers wrapped)))

;;; ================ part 2 ================

(defun sum-mid-matchers (l1 l2)
  (loop for i in l1
     for j in l2
     when (eq i j)
     sum i into summed
     finally (return (* 2 summed))))

;;; ================ main ================

(defun part2 ()
  (let* ((digits (puzzle-digits))
         (half-len (/ (length digits) 2)))
    (sum-mid-matchers (subseq digits 0 half-len) (subseq digits half-len))))
