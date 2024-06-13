;;; Fractal Art

(load "../utils.lisp")

(defun debug-display (art)
  (mapc (lambda (row) (format t "~a~%" (concatenate 'string row))) art))

(defun flip (key)
  (mapcar #'reverse key))

(defun diag-flip (key)
  (if (= (length key) 2)
      (list (list (second (second key)) (second (first key)))
            (list (first (second key)) (first (first key))))
      (list (list (third (third key)) (third (second key)) (third (first key)))
            (list (second (third key)) (second (second key)) (second (first key)))
            (list (first (third key)) (first (second key)) (first (first key))))))

(defun rotate (key n)
  (let ((k key))
    (dotimes (i n)
      (setf k (flip (diag-flip k))))
    k))

(defun store-rules (rules key val)
  (setf (gethash key rules) val)
  (setf (gethash (rotate key 1) rules) val)
  (setf (gethash (rotate key 2) rules) val)
  (setf (gethash (rotate key 3) rules) val)
  (let ((flipped-key (flip key)))
    (setf (gethash flipped-key rules) val)
    (setf (gethash (rotate flipped-key 1) rules) val)
    (setf (gethash (rotate flipped-key 2) rules) val)
    (setf (gethash (rotate flipped-key 3) rules) val)))

(defun word-to-val (word)
  (mapcar (lambda (s)
            (mapcar (lambda (str) (char str 0))
                    (cl-ppcre:split "" s)))
          (cl-ppcre:split "/" word)))

(defparameter beginning-pattern (word-to-val ".#./..#/###"))

(defun read-rules ()
  (let ((rules (make-hash-table  :test 'equal)))
    (mapc (lambda (line)
            (let* ((words (cl-ppcre:split " " line))
                   (key (word-to-val (first words)))
                   (val (word-to-val (third words))))
              (store-rules rules key val)))
          (input-lines 2017 21))
    rules))

(defun squarify (chunks)
  (format *error-output* "squarified starting with chunks ~a~%" chunks) ; DEBUG
  (let ((len (round (sqrt (length chunks)))))
    (loop for i from 0 to (- len 1)
       collect (loop for j from 0 to (- len 1)
                    collect (nth (+ (* i len) j) chunks)))))

(defun apply-rules (rules art)
  (format *error-output* "apply-rules~%") ; DEBUG
  (format *error-output* "art ~a~%" art)  ; DEBUG
  (debug-display art)                     ; DEBUG
  (let* ((chunk-size (if (evenp (length art)) 2 3))
         (num-chunks (/ (length art) chunk-size))
         (new-art-chunks nil))
    (dotimes (i num-chunks)
      (let ((rows (take (drop art (* i chunk-size)) chunk-size))
            (row-chunks nil))
        (dotimes (j num-chunks)
          (let ((chunk-key (mapcar (lambda (row) (take (drop row (* j chunk-size)) chunk-size)) rows)))
            (setf new-art-chunks (cons (gethash chunk-key rules) new-art-chunks))))
        (setf new-art-chunks (cons row-chunks new-art-chunks))))
    ;; now turn new-art-chunks into new art
    (format t "squarified: ~%~a~%" (squarify (car (reverse new-art-chunks))))
    (squarify (car (reverse new-art-chunks)))))

(defun count-on-pixels (art)
  (length (remove-if (lambda (ch) (eq ch #\.))
                     (alexandria:flatten art))))

(defun part1 ()
  (let ((rules (read-rules))
        (art beginning-pattern))
    (dotimes (i 5)
      (setf art (apply-rules rules art)))
    (count-on-pixels art)))
