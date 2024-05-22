;;;; Advent of Code utilities

(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)

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

;;; ================ data input ================

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

(defun data-file-path (year day part testing)
  "Returns a path to the data file for `year`, `day`, and `part` (may be nil).
If `testing` is non-nil, adds \"_test\" before the file extension."
  (let ((fname (format nil "day~2,'0D" day)))
    (when part (setq fname (cat fname (format nil "_~a" part))))
    (when testing (setq fname (cat fname "_test")))
    (format nil "../../data/y~a/~a.txt" year fname)))

(defun input-lines (year day part &key (testing nil) (keep-blank-lines nil))
  "INPUT-LINES calls INPUT-LINES-FROM with the path to the data file for
`year`, `day`, `part`, and optionally \"_test\" before the file extension.
Keeps blank lines if `keep-blank-lines` is non-nil.

If the file is not found and `part` > `, try with `part` = 1. If that is not
found, try it without a part number at all."
  (let ((path (data-file-path year day part testing)))
    (when (and (not (probe-file path)) (/= part 1))
      (setq path (data-file-path year day 1 testing)))
    (when (not (probe-file path))
      (setq path (data-file-path year day nil testing)))
    (input-lines-from path :keep-blank-lines keep-blank-lines)))

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
