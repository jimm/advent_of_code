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
    (when part (setq fname (concat fname (format nil "_~a" part))))
    (when testing (setq fname (concat fname "_test")))
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
