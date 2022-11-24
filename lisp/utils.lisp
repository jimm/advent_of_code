;;;; Advent of Code utilities

(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)

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

;;; ================ data input ================

(defvar *input-file-format* "../../data/y~a/day~2,'0D.txt")

(defun input-lines-from (path &key (keep-blank-lines nil))
  "INPUT-LINES reads the specified file and returns a list containing the
lines in the file, without newlines. Does not return blank lines unless
`:keep-blank-lines` is true."
  (with-open-file (f path)
    (let ((lines))
      (loop
         (let ((line (read-line f nil)))          ; return nil, not error, at EOF
           (unless line (return (reverse lines)))
           (if (or keep-blank-lines (not (equal line "")))
             (setf lines (cons line lines))))))))

(defun input-lines (y d &key (keep-blank-lines nil))
  "INPUT-LINES calls INPUT-LINES-FROM with the path to the data file for
year y and day d."
  (input-lines-from (format nil *input-file-format* y d)
                    :keep-blank-lines keep-blank-lines))

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
