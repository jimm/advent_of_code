;;;; Advent of Code utilities

(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :split-sequence)

(defun cat (&rest strings)
  (apply 'concatenate 'string strings))

(defun flatten (l) (alexandria:flatten l))

(defun drop (n l) (subseq l n))

(defun take (n l) (subseq l 0 n))

(defun to-integer (str)
  (if str
      (if (and (> (length str) 0) (or (digit-char-p (char str 0))
                                      (eq #\- (char str 0))))
          (with-input-from-string (s str) (read s))
          str)
      nil))

(defun split-by-space (string)
  "Returns a list of substrings of string divided by whitespace."
  (cl-ppcre:split "\\s+" string))

(defun split-by (regex string)
  "Returns a list of substrings of string divided by whitespace."
  (cl-ppcre:split regex string))

;;; ================ queue ================
;;; From Common Lisp Recipes (http://weitz.de/cl-recipes/), section 2-10.

(defclass queue()
  ((list :initform nil)
   (tail :initform nil)))

(defmethod dequeue ((queue queue))
  (with-slots (list) queue
    (pop list)))

(defmethod enqueue (new-item (queue queue))
  (with-slots (list tail) queue
    (let ((new-tail (list new-item)))
          (cond ((null list) (setf list new-tail))
                (t (setf (cdr tail) new-tail)))
          (setf tail new-tail)))
  queue)
