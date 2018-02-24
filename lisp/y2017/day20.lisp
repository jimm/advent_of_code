;;; Particle Swarm

(load "../utils.lisp")

(defstruct vec x y z)

(defun vec-length (vec)
  (+ (abs (vec-x vec))
     (abs (vec-y vec))
     (abs (vec-z vec))))

(defun vec-add! (vec1 vec2)
  "add vec2 to vec1, modifying vec1"
  (incf (vec-x vec1) (vec-x vec2))
  (incf (vec-y vec1) (vec-y vec2))
  (incf (vec-z vec1) (vec-z vec2)))

(defstruct particle pos vel acc num)

(defun particle-move (particle)
  (vec-add! (particle-vel particle) (particle-acc particle))
  (vec-add! (particle-pos particle) (particle-vel particle)))

(defun parse-vec (vec)
  (let ((nums (mapcar #'to-integer
                      (cl-ppcre:split "," (subseq vec 3 (- (length vec) 1))))))
    (make-vec :x (first nums) :y (second nums) :z (third nums))))

(defun parse-particle (line num)
  (let ((vecs (mapcar #'parse-vec (cl-ppcre:split ",? " line))))
    (make-particle :pos (first vecs) :vel (second vecs) :acc (third vecs) :num num)))

(defun read-particles ()
  (mapcar #'parse-particle
          (input-lines 2017 20)
          (loop for i from 0 to 9999 collecting i)))

;; ================ part 1 ================

(defun part1-guess-val (p)
  (+ (* 1000000 (vec-length (particle-acc p)))
     (*    1000 (vec-length (particle-vel p)))
                (vec-length (particle-pos p))))

(defun part1-guess-reducer (&optional p1 p2)
  (cond
    (p2 (if (< (part1-guess-val p1) (part1-guess-val p2)) p1 p2))
    (p1 p1)
    (t nil)))

(defun part1 ()
  (particle-num (reduce #'part1-guess-reducer (read-particles))))

;; ================ part 2 ================

(defun move-particles (particles)
  (mapc #'particle-move particles))

(defun remove-collisions (particles)
  (let ((pos-particles (make-hash-table :test #'equalp)))
    (mapc (lambda (p) (setf (gethash (particle-pos p) pos-particles)
                            (cons p (gethash (particle-pos p) pos-particles))))
                  particles)
    (remove-if (lambda (p)
                 (> (length (gethash (particle-pos p) pos-particles)) 1))
               particles)))

(defun part2 ()
  (let ((particles (read-particles)))
    (dotimes (i 1000)
      (setf particles (remove-collisions (move-particles particles))))
    (length particles)))
