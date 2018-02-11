;;; Disk Defragmentation

(load "../utils.lisp")
(load "day10.lisp")

(defparameter input "ugkiagan")
(defparameter test-input "flqrgnkx")
(defparameter hex-to-bits
  (let ((h (make-hash-table)))
    (setf
     (gethash #\0 h) '(0 0 0 0)
     (gethash #\1 h) '(0 0 0 1)
     (gethash #\2 h) '(0 0 1 0)
     (gethash #\3 h) '(0 0 1 1)
     (gethash #\4 h) '(0 1 0 0)
     (gethash #\5 h) '(0 1 0 1)
     (gethash #\6 h) '(0 1 1 0)
     (gethash #\7 h) '(0 1 1 1)
     (gethash #\8 h) '(1 0 0 0)
     (gethash #\9 h) '(1 0 0 1)
     (gethash #\a h) '(1 0 1 0)
     (gethash #\b h) '(1 0 1 1)
     (gethash #\c h) '(1 1 0 0)
     (gethash #\d h) '(1 1 0 1)
     (gethash #\e h) '(1 1 1 0)
     (gethash #\f h) '(1 1 1 1))
    h))

(defun hex-ch-to-bits (ch)
  (gethash ch hex-to-bits))

(defun hex-str-to-bits (hs)
  (loop for ch across hs
     collecting (hex-ch-to-bits ch) into bits
     finally (return (flatten bits))))

(defun make-grid (input)
  (let ((grid (make-array '(128 128) :element-type 'bit)))
    (dotimes (i 128)
      (let ((knot-bits (hex-str-to-bits
                        (day10-part2 256 (format nil "~a-~D" input i)))))
        (dotimes (j 128)
          (setf (aref grid i j) (nth j knot-bits)))))
    grid))

;; ================ part 1 ================

(defun count-ones (grid)
  (let ((num-ones 0))
    (dotimes (i 128)
      (dotimes (j 128)
        (if (= 1 (aref grid i j))
            (incf num-ones))))
    num-ones))

(defun day14-part1 ()
  (let ((grid (make-grid input)))
    (count-ones grid)))

;; ================ part 2 ================

(defun touching-ones-coords (grid x y)
  (let (tcs)
    (loop for delta in `((-1 0) (1 0) (0 -1) (0 1))
       do (let ((x1 (+ x (first delta)))
                (y1 (+ y (second delta))))
            (when (and
                   (array-in-bounds-p grid x1 y1)
                   (= 1 (aref grid x1 y1)))
              (setq tcs (cons (list x1 y1) tcs)))))
    tcs))

(defun remove-group-touching (grid seen)
  (loop while seen
     do (let* ((coord (first seen))
               (x (first coord))
               (y (second coord))
               (new-seen (rest seen)))
          (setf (aref grid x y) 0)
          (loop for tc in (touching-ones-coords grid x y)
             do (setf (aref grid (first tc) (second tc)) 0)
             when (not (member tc seen))
             do (setf new-seen (cons tc new-seen)))
          (setf seen new-seen)))
  grid)

(defun find-one-bit (grid)
  (loop for i from 0 to (- (* 128 128) 1)
     when (= 1 (row-major-aref grid i))
     return (list (truncate (/ i 128)) (mod i 128))))

(defun count-groups (grid &optional (count 0))
  (let ((one-bit-coord (find-one-bit grid)))
    (if one-bit-coord
        (count-groups (remove-group-touching grid (list one-bit-coord)) (+ 1 count))
        count)))

(defun day14-part2 ()
  (let ((grid (make-grid input)))
    (count-groups grid)))
