;;; Dueling Generators

(defparameter final-mod 2147483647)
(defstruct generator factor state filter)
(defparameter gen-a nil)
(defparameter gen-b nil)

(defun generator-init-a (f)
  (setf gen-a (make-generator :factor 16807 :state 679 :filter f)))
(defun generator-init-b (f)
  (setf gen-b (make-generator :factor 48271 :state 771 :filter f)))

(defun generator-next-value (g)
  (loop
     do (setf (generator-state g)
              (mod (* (generator-state g) (generator-factor g))
                   final-mod))
     when (funcall (generator-filter g) (generator-state g))
     return (generator-state g)))

(defun step-generators ()
  (generator-next-value gen-a)
  (generator-next-value gen-b))

(defun judge (fa fb n)
  (generator-init-a fa)
  (generator-init-b fb)
  (let ((count 0))
    (dotimes (_ n)
      (step-generators)
      (when (= (logand (generator-state gen-a) #xffff)
               (logand (generator-state gen-b) #xffff))
        (incf count)))
    count))

(defun part1 ()
  (judge (lambda (val) val)
         (lambda (val) val)
         40000000))

(defun part2 ()
  (judge (lambda (val) (= 0 (logand val 3)))
         (lambda (val) (= 0 (logand val 7)))
         5000000))
