;;; Digital Plumber

(defparameter test-lines
  (list
   "0 <-> 2"
   "1 <-> 1"
   "2 <-> 0, 3, 4"
   "3 <-> 2, 4"
   "4 <-> 2, 3, 6"
   "5 <-> 6"
   "6 <-> 4, 5"))

(defun parse-line (line)
  (let* ((parts (mapcar  (lambda (s) (string-trim '(#\space #\tab #\newline) s))
                         (split-by "<->" line)))
         (node (to-integer (first parts)))
         (other-nodes (mapcar #'to-integer (split-by ", " (second parts)))))
    (cons node other-nodes)))

(defun read-network (&optional lines)
  "Reads lines or data file and returns multiple values: a hash containing
the links from each node to other nodes, and the list of nodes (keys of the
hash)."
  (let ((h (make-hash-table))
        (parsed-lines (mapcar #'parse-line (or lines (input-lines 2017 12)))))
    (loop for pl in parsed-lines
       do (setf (gethash (first pl) h) (rest pl)))
    (values h (mapcar #'first parsed-lines))))

;; ================ part 1 ================

(defun do-group-members (h group-members)
  (let ((new-group-members
         (remove-duplicates
          (append group-members
                  (flatten (loop for m in group-members
                              collect (gethash m h)))))))
    (if (= (length new-group-members) (length group-members))
        group-members
        (do-group-members h new-group-members))))

(defun group-members (h k)
  (do-group-members h (cons k (gethash k h))))

(defun part1 ()
  (length (group-members (read-network) 0)))

(defun test-part1 ()
  (length (group-members (read-network test-lines) 0)))

;; ================ part 2 ================

(defun do-groups (h keys groups)
  (let* ((all-other-group-members (flatten groups))
         (non-group-member (find-if (lambda (k) (not (member k all-other-group-members))) keys)))
    (if non-group-member
        (do-groups h keys (cons (group-members h non-group-member) groups))
        groups)))

(defun groups (h keys)
  (do-groups h keys (list (group-members h 0))))

(defun part2 ()
  (multiple-value-bind (h keys) (read-network)
    (length (groups h keys))))

(defun test-part2 ()
  (multiple-value-bind (h keys) (read-network test-lines)
    (length (groups h keys))))

