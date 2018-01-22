;;; Hex Ed
;;; https://www.redblobgames.com/grids/hexagons/
;;; http://keekerdc.com/2011/03/hexagon-grids-coordinate-systems-and-distance-calculations/

(defun read-steps ()
  (split-by "," (first (input-lines 2017 11))))

(defun move (step x y z)
  (cond
    ((string= step "n") (list x (+ y 1) (- z 1)))
    ((string= step "s") (list x (- y 1) (+ z 1)))
    ((string= step "ne") (list (+ x 1) y (- z 1)))
    ((string= step "nw") (list (- x 1) (+ y 1) z))
    ((string= step "se") (list (+ x 1) (- y 1) z))
    ((string= step "sw") (list (- x 1) y (+ z 1)))))

(defun cube-distance (loc1 loc2)
  (/ (+ (abs (- (first loc1) (first loc2)))
        (abs (- (second loc1) (second loc2)))
        (abs (- (third loc1) (third loc2))))
     2))

(defun neighbors (x y z)
  (list
   (list x (+ y 1) (- z 1))
   (list x (- y 1) (+ z 1))
   (list (+ x 1) y (- z 1))
   (list (- x 1) (+ y 1) z)
   (list (+ x 1) (- y 1) z)
   (list (- x 1) y (+ z 1))))

(defun min-by (f vals)
  (loop for v in vals
     with min-f = 9999
     with min-f-val = nil
     do (when (< (funcall f v) min-f)
          (setf min-f (funcall f v))
          (setf min-f-val v))
     finally (return min-f-val)))

(defun do-astar-distance (loc goal steps)
  (if (equal loc goal)
      steps
      (let ((next-loc (min-by (lambda (x) (cube-distance x goal)) (apply #'neighbors loc))))
        (do-astar-distance next-loc goal (+ 1 steps)))))

(defun astar-distance (loc)
  (do-astar-distance loc (list 0 0 0) 0))

(defun travel-step (coord-and-max-dist step)
  (list (apply #'move (cons step (first coord-and-max-dist)))
        (max (second coord-and-max-dist)
             (astar-distance (first coord-and-max-dist)))))

(defun travel ()
  (reduce #'travel-step (read-steps) :initial-value (list (list 0 0 0) 0)))

(defun part1 ()
  (astar-distance (first (travel))))

(defun part2 ()
  (second (travel)))
