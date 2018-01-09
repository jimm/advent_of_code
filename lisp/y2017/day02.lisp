;;; Corruption Checksum

(defun read-input-nums ()
  (loop for line in (input-lines 2017 2)
     collect (loop for word in (split-by-space line)
                  collect (parse-integer word))))

;;; ================ part 1 ================

(defun minmax-diff (nums)
  (loop for num in nums
     maximize num into max
     minimize num into min
     finally (return (- max min))))

(defun part1 ()
  (apply #'+ (mapcar #'minmax-diff (read-input-nums))))

;;; ================ part 2 ================

(defun pairs (nums)
  (apply #'append
         (loop for i upto (- (length nums) 2)
            collect (loop for j from (+ i 1) upto (- (length nums) 1)
                       unless (eq i j)
                       collect (list (nth i nums) (nth j nums))))))

(defun max-min (ab)
  "Given a pair of numbers return them, max first."
  (let ((a (first ab)) (b (second ab)))
    (if (>= a b) (list a b) (list b a))))

(defun find-integer-divisor (nums)
  (let* ((combis (pairs nums))
         (first-evenly-divisible-pair
          (find-if (lambda (ab) (zerop (apply #'rem (max-min ab))))
                   combis)))
    (round (apply #'/ (max-min first-evenly-divisible-pair)))))

(defun part2 ()
  (apply #'+ (mapcar #'find-integer-divisor (read-input-nums))))
