;; Stream Processing

(defstruct stream-state
  (state 'group)
  (score 0)
  (total-score 0)
  (comment-count 0))

(defun score (str)
  (loop for c across str
     with state = (make-stream-state)
     do (cond
          ((eq (stream-state-state state) 'group)
           (case c
             (#\{ (incf (stream-state-score state)))
             (#\} (progn (setf (stream-state-total-score state)
                               (+ (stream-state-total-score state)
                                  (stream-state-score state)))
                         (decf (stream-state-score state))))
             (#\, nil)
             (#\< (setf (stream-state-state state) 'garbage))))
          ((eq (stream-state-state state) 'garbage)
           (cond
             ((eq c #\>) (setf (stream-state-state state) 'group))
             ((eq c #\!) (setf (stream-state-state state) 'skip))
             (t (incf (stream-state-comment-count state)))))
          ((eq (stream-state-state state) 'skip)
           (setf (stream-state-state state) 'garbage)))
          finally (return state)))

;; ================ testing ================

(defvar test-scores
  (list
   '("{}" . 1)
   '("{{{}}}" . 6)
   '("{{},{}}" . 5)
   '("{{{},{},{{}}}}" . 16)
   '("{<a>,<a>,<a>,<a>}" . 1)
   '("{{<ab>},{<ab>},{<ab>},{<ab>}}" . 9)
   '("{{<!!>},{<!!>},{<!!>},{<!!>}}" . 9)
   '("{{<a!>},{<a!>},{<a!>},{<ab>}}" . 3)))

(defun test-scoring ()
  (let ((failures nil))
    (mapc (lambda (test)
            (let ((score (score (car test))))
              (if (= (stream-state-total-score score) (cdr test))
                  (format t ".")
                  (progn
                    (format t "*")
                    (setf failures (cons (list test (cdr test) score) failures))))))
          test-scores)
    (format t "~%")
    (mapc (lambda (failure)
            (format t "expected ~D, got ~D: ~a~%" (second failure)
                    (stream-state-total-score (third failure)) (first failure)))
          (reverse failures))))

;; ================ main ================

(defun part0 (f)
  (apply #'+ (mapcar (lambda (line) (funcall f (score line)))
                     (input-lines 2017 9))))

(defun part1 () (part0 #'stream-state-total-score))

(defun part2 () (part0 #'stream-state-comment-count))
