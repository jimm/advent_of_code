;;;; Advent of Code utilities

(ql:quickload :cl-ppcre)

(defun to-integer (str)
  (with-input-from-string (s str) (read s)))

(defun split-by-space (string)
  "Returns a list of substrings of string divided by whitespace."
  (cl-ppcre:split "\\s+" string))

(defun split-by (regex string)
  "Returns a list of substrings of string divided by whitespace."
  (cl-ppcre:split regex string))

(defvar *input-file-format* "../../data/y~a/day~2,'0D.txt")

(defun input-lines-from (path &key (keep-blank-lines nil))
  "INPUT-LINES reads the speicified file and returns a list containing the
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
