;;; Memory Reallocation

(defun read-banks ()
  (apply #'vector
         (mapcar #'to-integer (split-by-space (car (input-lines 2017 6))))))

(defun seen-before-p (banks state)
  (gethash banks state))

(defun next-index (index len)
  (let ((ni (+ index 1)))
    (if (>= ni len) 0 ni)))

(defun do-redistribute (banks len max i)
  (if (zerop max)
      banks
      (let ((val (aref banks i)))
        (setf (aref banks i) (+ val 1))
        (do-redistribute
            banks
          len
          (- max 1)
          (next-index i len)))))

(defun redistribute (banks)
  (let* ((max (apply #'max (coerce banks 'list)))
         (max-index (position max banks :test #'eq)))
    (setf (aref banks max-index) 0)
    (do-redistribute
        banks
      (length banks)
      max
      (next-index max-index (length banks)))))

(defun cycle-until-repeat (banks)
  (let ((cycles 0)
        (state (make-hash-table :test #'equalp)))
    (loop
       ;; we need to copy banks because it's being used as a hash key so
       ;; we can't modify it
       do (setf banks (redistribute (copy-seq banks)))
       when (seen-before-p banks state)
         return (values (+ cycles 1) (- cycles (gethash banks state)))
       end
       do (progn
         (setf (gethash banks state) cycles)
         (incf cycles)))))

(defun part1 ()
  (cycle-until-repeat (read-banks)))

(defun test-part1 ()
  (multiple-value-list (cycle-until-repeat (vector 0 2 7 0))))

(defun part2 ()
  (second (multiple-value-list (cycle-until-repeat (read-banks)))))
