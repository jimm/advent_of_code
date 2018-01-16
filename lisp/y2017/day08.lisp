;;; I Heard You Like Registers

(defstruct cpu regs reg-names max-seen)

(defstruct instruction reg op const if)

(defun parse-line (line)
  (let ((words (split-by-space line)))
    (make-instruction
     :reg (first words)
     :op (second words)
     :const (to-integer (third words))
     :if (make-instruction
          :reg (fifth words)
          :op (sixth words)
          :const (to-integer (seventh words))))))

(defun read-reg (reg cpu)
  (gethash reg (cpu-regs cpu) 0))

(defun write-reg (reg val cpu)
  (setf (cpu-max-seen cpu)              ; remember max val seen
        (max val (cpu-max-seen cpu)))
  (setf (gethash reg (cpu-regs cpu)) val) ; value
  (unless (member reg (cpu-reg-names cpu)) ; remember reg name
    (setf (cpu-reg-names cpu) (cons reg (cpu-reg-names cpu))))
  val)

(defun instructions ()
  (mapcar #'parse-line (input-lines 2017 8)))

(defun eval-instruction (inst cpu)
  (let* ((reg (instruction-reg inst))
         (op (instruction-op inst))
         (reg-val (read-reg reg cpu))
         (const (instruction-const inst)))
    (cond
      ((string= op "==") (= reg-val const))
      ((string= op "!=") (not (= reg-val const)))
      ((string= op ">") (> reg-val const))
      ((string= op ">=") (>= reg-val const))
      ((string= op "<") (< reg-val const))
      ((string= op "<=") (<= reg-val const))
      ((string= op "dec") (write-reg reg (- reg-val const) cpu))
      ((string= op "inc") (write-reg reg (+ reg-val const) cpu)))))

(defun run-instruction (inst cpu)
  (when (eval-instruction (instruction-if inst) cpu)
    (eval-instruction inst cpu)))

(defun run ()
  (let ((cpu (make-cpu
              :regs (make-hash-table :test #'equal)
              :max-seen -9999)))
    (mapc (lambda (inst) (run-instruction inst cpu))
          (instructions))
    cpu))

(defun part1 ()
  (let ((cpu (run)))
    (apply #'max (mapcar (lambda (r) (read-reg r cpu))
                         (cpu-reg-names cpu)))))

(defun part2 ()
  (let ((cpu (run)))
    (cpu-max-seen cpu)))
