;;; Spiral Memory

(defvar start-location 368078)
(defvar max-size (+ 1 (round (sqrt start-location))))
(defvar state-coord-offset (truncate (/ max-size 2)))

(defun sqrt-of-bottom-corner-containing (n)
  (let ((isqrt (truncate (sqrt n))))
    (if (oddp isqrt)
        (if (eq n (* isqrt isqrt)) isqrt (+ 2 isqrt))
        (+ 1 isqrt))))

(defun max-coord (sqrt) (truncate (/ sqrt 2)))

(defun find-coord (i bot-right sqrt mc)
  (let ((sqrtm1 (- sqrt 1)))
    ;; use method and pattern match?
    (if (>= i (- bot-right sqrtm1))
        ;; bottom row
        (list (- mc (- bot-right i)) (- mc))
        (if (>= i (- bot-right (* 2 sqrtm1)))
            ;; left row
            (list (- mc) (+ (- (max-coord sqrt))
                            (- bot-right sqrtm1 i)))
            (if (>= i (- bot-right (* 3 sqrtm1)))
                ;; top row
                (list (+ (- mc) (- bot-right (* 2 sqrtm1) i)) mc)
                ;; right row
                (list mc (- mc (- bot-right (* 3 sqrtm1) i))))))))

(defun coord-of (n)
  (let ((sqrt (sqrt-of-bottom-corner-containing n)))
    (find-coord n (* sqrt sqrt) sqrt (max-coord sqrt))))

;; ================ part 1 ================

(defun manhattan-distance-to (i)
  (let ((coord (coord-of i)))
    (+ (abs (first coord)) (abs (second coord)))))

(defun manhattan-distance-to-start ()
  (manhattan-distance-to start-location))

(defun part1 ()
  (manhattan-distance-to-start))

;; ================ part 2 ================

;; FIXME

(defmacro bit-at (state x y)
  `(aref ,state (+ ,x state-coord-offset) (+ ,y state-coord-offset)))

(defun sum-of-surrounding (xy state)
  (destructuring-bind (x y) xy
    (loop for cx from -1 to 2
       for cy from -1 to 2
       when (not (and (= cx 0) (= cy 0)))
       summing (bit-at state (+ x cx) (+ y cy)) into sum
       finally (return sum))))

(defun stress-test-value (i state)
  (let* ((coord (coord-of i))
         (val (sum-of-surrounding coord state)))
    (if (> val start-location)
        val
        (progn
          (destructuring-bind (x y) coord
            (setf (bit-at state x y) val)
            (stress-test-value (+ 1 i) state))))))

(defun part2 ()
  (let* ((max-size (+ 1 (truncate (sqrt start-location))))
         (state (make-array (list max-size max-size) :element-type 'integer :initial-element 0)))
    (setf (bit-at state 0 0) 1)
    (stress-test-value 2 state)))
