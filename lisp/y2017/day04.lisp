;;; High-Entropy Passphrases

(defun count-valid (f)
  (count-if f (mapcar #'split-by-space (input-lines 2017 4))))

;; ================ part 1 ================

(defun words-valid-p (words)
  (eq (length words)
      (length (remove-duplicates words :test #'equal))))

(defun part1 ()
  (count-valid #'words-valid-p))

;; ================ part 2 ================

(defun sorted-words-valid-p (words)
  (let ((sorted-words (mapcar (lambda (word) (sort word #'char-lessp)) words)))
    (eq (length sorted-words)
        (length (remove-duplicates sorted-words :test #'equal)))))

(defun part2 ()
  (count-valid #'sorted-words-valid-p))
