;;; Spinlock

(defparameter input 328)
(defparameter test-input 3)
(defparameter last-insert 2017)

(defstruct circular-buffer vals idx len after-zero)

(defun init-circular-buffer ()
  (make-circular-buffer :vals '(0) :idx 0 :len 1))

(defun circular-buffer-next-pos (cbuf n)
  (mod (+ (circular-buffer-idx cbuf) n)
       (circular-buffer-len cbuf)))

(defun forward-and-insert-after (cbuf n i)
  (let* ((vals (circular-buffer-vals cbuf))
         (pos (circular-buffer-next-pos cbuf n)))
    (setf vals (alexandria:rotate vals (- pos)))
    (setf vals (cons i vals))
    (setf (circular-buffer-vals cbuf) (alexandria:rotate vals pos))
    (setf (circular-buffer-idx cbuf) (+ 1 pos))
    (incf (circular-buffer-len cbuf))))

(defun move-remember-after-zero (cbuf n i)
  (let ((pos (circular-buffer-next-pos cbuf n)))
    (when (= 0 pos)
      (setf (circular-buffer-after-zero cbuf) i))
    (setf (circular-buffer-idx cbuf) (+ 1 pos))
    (incf (circular-buffer-len cbuf))))

(defun value-after (cbuf val)
  (loop
     with vals = (circular-buffer-vals cbuf)
     when (= (first vals) val)
       return (first (rest vals))
     end
     do (setf vals (rest vals))))

(defun print-first (cbuf n)
  (format t "~a~%" (subseq (circular-buffer-vals cbuf) 0 n)))

(defun part1 (&optional (steps input))
  (let ((cbuf (init-circular-buffer)))
    (dotimes (i last-insert)
      (forward-and-insert-after cbuf steps (+ i 1)))
    (value-after cbuf last-insert)))

(defun part2 (&optional (steps input))
  (let ((cbuf (init-circular-buffer)))
    (dotimes (i 50000000)
      (move-remember-after-zero cbuf steps (+ i 1)))
    (circular-buffer-after-zero cbuf)))
