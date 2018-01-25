;;; Packet Scanners

(defun parse-line (line)
  (let ((depth-range (mapcar (lambda (s)
                               (to-integer
                                (string-trim (list #\space #\tab #\newline) s)))
                             (split-by ": " line))))
    (make-layer :depth (first depth-range) :range (second depth-range))))

(defun read-firewall ()
  (mapcar #'parse-line (input-lines 2017 13)))

;; ================ layer ================

(defstruct layer depth range (loc 0) (direction 'down))

(defun layer-down-p (layer) (eq (layer-direction layer) 'down))
(defun layer-up-p   (layer) (eq (layer-direction layer) 'up))

(defun layer-cost (layer)
  (* (layer-depth layer) (layer-range layer)))

(defun layer-hit-check-cost (layer)
  (if (and layer (= 0 (layer-loc layer)))
      (layer-cost layer)
      0))

(defun layer-step (layer)
  (cond ((eq nil layer) nil)
        ;; going down, at bottom
        ((and (layer-down-p layer) (= (layer-loc layer) (- (layer-range layer) 2)))
         (incf (layer-loc layer))
         (setf (layer-direction layer) 'up))
        ;; going down
        ((layer-down-p layer)
         (incf (layer-loc layer)))
        ;; going up, at top
        ((and (layer-up-p layer) (= (layer-loc layer) 1))
         (setf (layer-loc layer) 0
               (layer-direction layer) 'down))
        ;; going up
        ((layer-up-p layer)
         (decf (layer-loc layer))))
  layer)

(defun layer-at-top-step-p (layer step)
  "Returns true if layer is at the top (position zero) at the specified
step. Periodicity is (range + (range - 2))."
  (cond
    ((= step 0) t)
    ((eq nil layer) nil)
    (t (= 0 (mod step (+ (layer-range layer) (layer-range layer) -2))))))

;; ================ firewall ================

(defun firewall-max-depth (firewall)
  (loop for layer in firewall
     maximizing (layer-depth layer) into max-depth
     finally (return max-depth)))

(defun firewall-layer (firewall i)
  (find-if (lambda (l) (and l (= i (layer-depth l))))
           firewall))

(defun firewall-step (firewall)
  (mapc #'layer-step firewall))

;; ================ part 1 ================

(defun part1 ()
  (let ((firewall (read-firewall))
        (cost 0))
    (dotimes (i (+ 1 (firewall-max-depth firewall)))
      (incf cost (layer-hit-check-cost (firewall-layer firewall i)))
      (firewall-step firewall))
    cost))

;; ================ part 2 ================

(defun no-hits-p (firewall delay)
  (eq nil
      (find-if (lambda (layer)
                 (layer-at-top-step-p layer (+ delay (layer-depth layer))))
               firewall)))

(defun part2 ()
  (loop
     with firewall = (read-firewall)
     with delay = 0
     when (no-hits-p firewall delay) return delay
     do (incf delay)))

;; ================ testing ================

(defun l (depth range loc dir)
  (make-layer :depth depth :range range :loc loc :direction dir))

(defun test-layers ()
  (assert (equalp (l 0 3 1 'down) (layer-step (l 0 3 0 'down))))
  (assert (equalp (l 0 3 2 'up) (layer-step (l 0 3 1 'down))))
  (assert (equalp (l 0 3 1 'up) (layer-step (l 0 3 2 'up))))
  (assert (equalp (l 0 3 0 'down) (layer-step (l 0 3 1 'up))))
  (assert (equalp (l 0 3 1 'down)
                  (let ((layer (l 0 3 0 'down)))
                    (dotimes (i 5) (layer-step layer))
                    layer)))
  t)
