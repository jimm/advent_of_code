;;; Duet

(load "../utils.lisp")

(define-condition nonzero-received (error)
  ((value-received :initarg :value-received
                   :reader value-received))
  (:report (lambda (condition stream)
             (format stream "nonzero value ~D received" (value-received condition)))))

(define-condition deadlock (error)
  ()
  (:report (lambda (condition stream)
             (format stream "deadlock" ))))

;; ================ cpu ================

(defstruct cpu name pc mem snd-func rcv-func send-count mailbox io-blocked program)

(defun cpu-debug-print (cpu)
  (format t "cpu ~s~%"(cpu-name cpu))
  (format t "  pc: ~d~%"(cpu-pc cpu))
  (format t "  memory:~%")
  (maphash (lambda (k v) (format t "    ~a: ~d~%" k v)) (cpu-mem cpu))
  (format t "  snd: ~a~%" (cpu-snd-func cpu))
  (format t "  rcv: ~a~%" (cpu-rcv-func cpu))
  (format t "  send-count: ~d~%" (cpu-send-count cpu))
  (format t "  mailbox: ~a~%" (cpu-mailbox cpu))
  (format t "  io-blocked: ~a~%" (cpu-io-blocked cpu)))

(defun cpu-load-program (cpu program)
  (setf (cpu-program cpu) program))

(defun cpu-read (cpu ref)
  (if (numberp ref) ref
      (gethash ref (cpu-mem cpu) 0)))

(defun cpu-write (cpu ref val)
  (setf (gethash ref (cpu-mem cpu)) val)
  val)

(defun incr-pc (cpu &optional (delta 1))
  (incf (cpu-pc cpu) delta)
  cpu)

(defun cpu-set (cpu ref1 ref2)
  (cpu-write cpu ref1 (cpu-read cpu ref2))
  (incr-pc cpu))

(defun cpu-add (cpu ref1 ref2)
  (cpu-write cpu ref1 (+ (cpu-read cpu ref1)
                         (cpu-read cpu ref2)))
  (incr-pc cpu))

(defun cpu-mul (cpu ref1 ref2)
  (cpu-write cpu ref1 (* (cpu-read cpu ref1)
                         (cpu-read cpu ref2)))
  (incr-pc cpu))

(defun cpu-mod (cpu ref1 ref2)
  (cpu-write cpu ref1 (mod (cpu-read cpu ref1)
                           (cpu-read cpu ref2)))
  (incr-pc cpu))

(defun cpu-jgz (cpu ref1 ref2)
  (incr-pc cpu (if (> (cpu-read cpu ref1) 0)
                   (cpu-read cpu ref2) 1)))

(defun cpu-snd (cpu ref1 ref2)
  (incf (cpu-send-count cpu))
  (funcall (cpu-snd-func cpu) cpu ref1 ref2))

(defun cpu-rcv (cpu ref1 ref2)
  (funcall (cpu-rcv-func cpu) cpu ref1 ref2))

(defun execute (cpu instruction)
  (let ((f (first instruction))
        (ref1 (second instruction))
        (ref2 (third instruction)))
    (funcall f cpu ref1 ref2)))

(defun execute-instruction-at-pc (cpu)
  (let ((instruction (aref (cpu-program cpu) (cpu-pc cpu))))
    (execute cpu instruction)))

(defun cpu-create (name snd-func rcv-func &optional (p-val 0))
  (let ((cpu (make-cpu :name name :pc 0 :mem (make-hash-table :test #'equal)
                       :snd-func snd-func :rcv-func rcv-func
                       :send-count 0 :mailbox (make-instance 'queue)
                       :io-blocked nil :program nil)))
    (cpu-write cpu #\p p-val)
    cpu))

;; ================ reading program ================

(defun opcode-to-function (word)
  (let ((funcname (concatenate 'string "CPU-" (string-upcase word))))
    (find-symbol funcname)))

(defun parse-word (word)
  (if word
      (let ((first-char (char word 0)))
        (cond ((or (digit-char-p first-char) (eq #\- first-char))
               (to-integer word))
              ((= 1 (length word))
               first-char)
              (t word)))
      nil))

(defun parse-instruction (line)
  (let ((words (cl-ppcre:split "\\s+" line)))
    (list (opcode-to-function (first words))
          (parse-word (second words))
          (parse-word (third words)))))

(defun read-program ()
  "Returns a vector of instructions."
  (let ((lines (input-lines 2017 18)))
    (map-into (make-array (length lines))
              #'parse-instruction
              (input-lines 2017 18))))

;; ================ part1 ================

(defun run-until-recovery (cpu)
  (handler-case
      (loop do (execute-instruction-at-pc cpu))
    (nonzero-received ()
      cpu)))

(defun part1-snd (cpu ref1 ref2)
  (cpu-write cpu "played" (cpu-read cpu ref1))
  (incr-pc cpu))

(defun part1-rcv (cpu ref1 ref2)
  (if (zerop (cpu-read cpu ref1))
      (incr-pc cpu)
      (let ((played (cpu-read cpu "played")))
        (cpu-write cpu "received" played)
        (signal (make-condition 'nonzero-received :value-received played)))))

(defun part1 ()
  (let ((cpu (cpu-create "p1c1" #'part1-snd #'part1-rcv)))
    (cpu-load-program cpu (read-program))
    (run-until-recovery cpu)
    (cpu-read cpu "received")))

;; ================ part2 ================

(defun deadlocked? (cpus)
  (every #'cpu-io-blocked cpus))

(defun part2-snd (cpu ref1 ref2)
  (enqueue (cpu-read cpu ref1) (cpu-mailbox (cpu-read cpu "other-cpu")))
  (incr-pc cpu))

(defun part2-rcv (cpu ref1 ref2)
  (let ((mail (dequeue (cpu-mailbox cpu))))
    (cond
      (mail
       ;; we've got mail!
       (progn
         (setf (cpu-io-blocked cpu) nil)
         (cpu-write cpu ref1 mail)
         (incr-pc cpu)))
      ((deadlocked? (list cpu (cpu-read cpu "other-cpu")))
       ;; deadlocked
       (signal (make-condition 'deadlock)))
      (t
       ;; no mail yet; blocked waiting
       (setf (cpu-io-blocked cpu) t)))))

(defun run-until-deadlock (cpus)
  (handler-case
      (loop do (mapc #'execute-instruction-at-pc cpus))
    (deadlock ()
      cpus)))

;; answer we get here is too high
(defun part2 ()
  (let ((cpu1 (cpu-create "p2c1" #'part2-snd #'part2-rcv 0))
        (cpu2 (cpu-create "p2c2" #'part2-snd #'part2-rcv 1))
        (program (read-program)))
    (cpu-write cpu1 "other-cpu" cpu2)
    (cpu-write cpu2 "other-cpu" cpu1)
    (cpu-load-program cpu1 program)
    (cpu-load-program cpu2 program)
    (run-until-deadlock (list cpu1 cpu2))
    (cpu-send-count cpu2)))
