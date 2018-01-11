;;; A Maze of Twisty Trampolines, All Alike

(defun do-steps-until-out-of-bounds (vec len idx steps f)
  (loop
     when (or (minusp idx) (>= idx len))
       return steps
     end
     do
       (let ((offset (aref vec idx)))
         (setf (aref vec idx) (funcall f offset))
         (setf idx (+ idx offset))
         (setf steps (+ 1 steps)))))

(defun steps-until-out-of-bounds (f &optional input-nums)
  (let* ((nums (or input-nums (mapcar #'to-integer (input-lines 2017 5))))
         (vec (make-array (length nums) :initial-contents nums
                          :element-type 'integer)))
    (do-steps-until-out-of-bounds vec (length nums) 0 0 f)))

(defun part1 ()
  (steps-until-out-of-bounds (lambda (offset) (+ 1 offset))))

(defun test-part1 ()
  (steps-until-out-of-bounds (lambda (offset) (+ 1 offset))
                             (list 0 3 0 1 -3)))

(defun part2 ()
  (steps-until-out-of-bounds
   (lambda (offset)
     (if (>= offset 3) (- offset 1) (+ offset 1)))))
