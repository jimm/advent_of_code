;;; Knot Hash

(defparameter rope-len 256)
(defparameter test-rope-len 5)
(defparameter test-input (list 3 4 1 5))
(defparameter input-suffix (list 17 31 73 47 23))

(defun read-input-lengths ()
  (mapcar #'to-integer
          (split-by "," (first (input-lines 2017 10)))))

(defun init-rope (len)
  (apply #'vector (loop for i upto (- len 1)
                     collect i)))

(defun reverse-subseq (rope rope-len index len)
  (let ((doubled (concatenate 'vector rope rope)))
    (setf (subseq doubled index (+ index len))
          (reverse (subseq doubled index (+ index len))))
    (let ((over-len (- (+ index len) rope-len)))
      (when (plusp over-len)
        (setf (subseq doubled 0 over-len)
              (subseq doubled rope-len (+ rope-len over-len)))))
    (subseq doubled 0 rope-len)))

(defun knot (rope rope-len lens index skip-size)
  (if (eq lens nil)
      ;; return args to make part2 looping simpler
      (list rope rope-len lens index skip-size)
      (knot
       (reverse-subseq rope rope-len index (first lens))
       rope-len
       (rest lens)
       (mod (+ index (first lens) skip-size) rope-len)
       (+ 1 skip-size))))

(defun xor-block (xs)
  (loop for x in (rest xs)
       with val = (first xs)
     do (setf val (logxor val x))
     finally (return val)))

(defun consolidate (sparse-vec &optional (dense '()))
  (if (plusp (length sparse-vec))
      (let ((block (subseq sparse-vec 0 16)))
        (consolidate (subseq sparse-vec 16)
                     (cons (xor-block (coerce block 'list)) dense)))
      (reverse dense)))

(defun dense-to-hex-str (dense)
  (string-downcase
   (apply #'concatenate (cons 'string (mapcar (lambda (d) (format nil "~2,'0x" d)) dense)))))

(defun part1 (&optional rope-length lens)
  (let* ((rope (init-rope (or rope-length rope-len)))
         (knot (knot rope (length rope) (or lens (read-input-lengths)) 0 0)))
    (* (elt (first knot) 0) (elt (first knot) 1))))

(defun test-part1 ()
  (part1 5 (list 3 4 1 5)))

(defun part2 (&optional rope-length input-str)
  (let* ((rope (init-rope (or rope-length rope-len)))
         (lengths
          (append
           (loop for c across (or input-str (first (input-lines 2017 10)))
              collect (char-int c))
           input-suffix))
         (index 0)
         (step 0))
    (dotimes (i 64)
      (let ((new-knot (knot rope (length rope) lengths index step)))
        (setf rope (first new-knot))
        (setf index (fourth new-knot))
        (setf step (fifth new-knot))))
    (dense-to-hex-str (consolidate rope))))
