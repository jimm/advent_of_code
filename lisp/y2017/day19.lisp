;;; A Series of Tubes

(load "../utils.lisp")

(defstruct tuber tubes row col dir steps)

(defun tuber-char-at (tuber row col)
  (handler-case
      (aref (tuber-tubes tuber) row col)
    (sb-int:invalid-array-index-error () #\space)))

(defun tuber-char (tuber)
  (tuber-char-at tuber (tuber-row tuber) (tuber-col tuber)))

(defun tuber-at-end? (tuber)
  (or (minusp (tuber-row tuber))
      (minusp (tuber-col tuber))
      (>= (tuber-row tuber) (array-dimension (tuber-tubes tuber) 0))
      (>= (tuber-col tuber) (array-dimension (tuber-tubes tuber) 1))
      (eq #\space (tuber-char tuber))))

(defun tuber-change-direction (tuber)
  (let ((h (make-hash-table)))
    (setf (gethash :up h)    (tuber-char-at tuber (- (tuber-row tuber) 1) (tuber-col tuber))
          (gethash :down h)  (tuber-char-at tuber (+ (tuber-row tuber) 1) (tuber-col tuber))
          (gethash :left h)  (tuber-char-at tuber (tuber-row tuber) (- (tuber-col tuber) 1))
          (gethash :right h) (tuber-char-at tuber (tuber-row tuber) (+ (tuber-col tuber) 1)))
    (setf (gethash (case (tuber-dir tuber)
                     (:up :down)
                     (:down :up)
                     (:left :right)
                     (:right :left))
                   h)
          #\space)
    (setf (tuber-dir tuber)
          (loop for dir in (list :up :down :left :right)
             when (not (eq (gethash dir h) #\space))
             return dir))))

(defun tuber-move (tuber)
  (when (eq #\+ (tuber-char tuber))
    (tuber-change-direction tuber))
  (incf (tuber-steps tuber))
  (case (tuber-dir tuber)
    (:up    (decf (tuber-row tuber)))
    (:down  (incf (tuber-row tuber)))
    (:left  (decf (tuber-col tuber)))
    (:right (incf (tuber-col tuber)))))

(defun tuber-start-pos (tubes)
  (list 0 (loop for i from 0 to (- (array-dimension tubes 1) 1)
             when (eq #\| (aref tubes 0 i))
             return i)))

(defun tuber-create ()
  (let* ((tuber (make-tuber :tubes (read-tubes) :dir :down :steps 0))
         (pos (tuber-start-pos (tuber-tubes tuber))))
    (setf (tuber-row tuber) (first pos)
          (tuber-col tuber) (second pos))
    tuber))

(defun read-tubes ()
  (let* ((lines (input-lines 2017 19))
         (tubes (make-array (list (length lines) (length (first lines)))
                            :element-type 'character)))
    (loop for line in lines
       for row-index from 0 to (- (length lines) 1)
       do (loop for ch across line
             for col-index from 0 to (- (length line) 1)
             do (setf (aref tubes row-index col-index) ch)))
    tubes))

(defun letters-on-path (tuber &rest chars)
  (loop
     when (tuber-at-end? tuber)
       return (concatenate 'string (reverse chars))
     end
     do (progn
          (tuber-move tuber)
          (let ((ch (tuber-char tuber)))
            (when (alphanumericp ch)
              (setq chars (cons ch chars)))))))

(defun path-length (tuber)
  (loop
     when (tuber-at-end? tuber)
       return (tuber-steps tuber)
     end
     do (tuber-move tuber)))

(defun part1 ()
  (letters-on-path (tuber-create)))

(defun part2 ()
  (path-length (tuber-create)))
