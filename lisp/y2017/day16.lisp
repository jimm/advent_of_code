;;; Permutation Promenade

(load "../utils.lisp")

(defun dance-start ()
  (let ((dance-line (make-array 16 :element-type 'character)))
    (loop for i from 0 to 15
       with a-code = (char-code #\a)
       do (setf (aref dance-line i) (code-char (+ a-code i))))
    dance-line))

(defun read-moves ()
  (mapcar (lambda (s)
            (let ((op (char s 0))
                  (args (cl-ppcre:split "/" (subseq s 1))))
              (list op
                    (if (eq #\p op)
                        (char (first args) 0)
                        (to-integer (first args)))
                    (if (eq #\p op)
                        (char (second args) 0)
                        (to-integer (second args))))))
          (cl-ppcre:split "," (first (input-lines 2017 16)))))

(defun spin (dance-line n)
  (alexandria:rotate dance-line n))

(defun swap-positions (dance-line i j)
  (let ((tmp (aref dance-line i)))
    (setf (aref dance-line i) (aref dance-line j))
    (setf (aref dance-line j) tmp)))

(defun swap-programs (dance-line a b)
  (swap-positions dance-line
                  (position a dance-line)
                  (position b dance-line)))

(defun move (dance-line move)
  (case (first move)
    (#\s (spin dance-line (second move)))
    (#\x (swap-positions dance-line (second move) (third move)))
    (#\p (swap-programs dance-line (second move) (third move)))))

(defun dance (dance-line moves)
  (mapc (lambda (m) (move dance-line m)) moves))

;; ================ part 1 ================

(defun part1 ()
  (let ((dance-line (dance-start)))
    (dance dance-line (read-moves))
    (concatenate 'string dance-line)))

;; ================ part 2 ================

(defun generate-dance-cycle (dance-line moves &optional (cycle nil))
  (let ((next-line (make-array 16 :element-type 'character)))
    (dance dance-line moves)
    (map-into next-line #'identity dance-line)
    (if (member next-line cycle :test #'equal)
        (reverse cycle)
        (generate-dance-cycle dance-line moves (cons next-line cycle)))))

(defun part2 ()
  (let* ((dance-line (dance-start))
         (moves (read-moves))
         (cycle (generate-dance-cycle dance-line moves)))
    (setf dance-line (dance-start))
    (dotimes (i (mod 1000000000 (length cycle)))
      (dance dance-line moves))
    (concatenate 'string dance-line)))
