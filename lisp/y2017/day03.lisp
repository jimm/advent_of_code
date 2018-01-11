;;; Spiral Memory

(defparameter start-location 368078)
(defvar state-coord-offset)

(defun sqrt-of-bottom-corner-containing (n)
  (let ((isqrt (truncate (sqrt n))))
    (if (oddp isqrt)
        (if (eq n (* isqrt isqrt)) isqrt (+ 2 isqrt))
        (+ 1 isqrt))))

(defun max-coord (sqrt) (truncate (/ sqrt 2)))

(defun find-coord (i bot-right sqrt mc)
  (let ((sqrtm1 (- sqrt 1)))
    (cond ((>= i (- bot-right sqrtm1))  ; bottom row
           (list (- mc (- bot-right i))
                 (- mc)))
          ((>= i (- bot-right (* 2 sqrtm1))) ; left row
           (list (- mc)
                 (+ (- (max-coord sqrt)) (- bot-right sqrtm1 i))))
          ((>= i (- bot-right (* 3 sqrtm1))) ; top row
           (list (+ (- mc) (- bot-right (* 2 sqrtm1) i))
                 mc))
          (t                            ; right row
           (list mc
                 (- mc (- bot-right (* 3 sqrtm1) i)))))))

;; (defun find-coord (i bot-right sqrt mc)
;;   (let ((sqrtm1 (- sqrt 1)))
;;     ;; use method and pattern match?
;;     (if (>= i (- bot-right sqrtm1))
;;         ;; bottom row
;;         (list (- mc (- bot-right i)) (- mc))
;;         (if (>= i (- bot-right (* 2 sqrtm1)))
;;             ;; left row
;;             (list (- mc) (+ (- (max-coord sqrt))
;;                             (- bot-right sqrtm1 i)))
;;             (if (>= i (- bot-right (* 3 sqrtm1)))
;;                 ;; top row
;;                 (list (+ (- mc) (- bot-right (* 2 sqrtm1) i)) mc)
;;                 ;; right row
;;                 (list mc (- mc (- bot-right (* 3 sqrtm1) i))))))))

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

(defmacro num-at (state x y)
  `(aref ,state (+ ,x state-coord-offset) (+ ,y state-coord-offset)))

(defun sum-of-surrounding (xy state)
  (destructuring-bind (x y) xy
    (loop for cx from -1 to 1
       summing (loop for cy from -1 to 1
                  when (not (and (= cx 0) (= cy 0)))
                  summing (num-at state (+ x cx) (+ y cy)) into sum
                  finally (return sum))
       into outer-sum
       finally (return outer-sum))))

(defun stress-test-value (i state)
  (let* ((coord (coord-of i))
         (val (sum-of-surrounding coord state)))
    (if (> val start-location)
        val
        (progn
          (destructuring-bind (x y) coord
            (setf (num-at state x y) val)
            (stress-test-value (+ 1 i) state))))))

(defun part2 ()
  (let* ((start-loc (coord-of start-location))
         (max-dim (* 2 (+ (apply #'max (mapcar #'abs start-loc)) 4)))
         (state (make-array (list max-dim max-dim)
                            :element-type 'integer
                            :initial-element 0)))
    (setf state-coord-offset (round (/ max-dim 2)))
    (setf (num-at state 0 0) 1)
    (stress-test-value 2 state)))
