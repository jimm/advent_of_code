#!/usr/bin/env janet
#
# Chronospatial Computer

(import ../running)

# ================ helpers ================

(defn read-computer
  [lines]
  (def comp @{:a 0 :b 0 :c 0 :pc 0 :mem nil :out @[]})
  (each line lines
    (cond
      (= "Register A: " (slice line 0 12))
      (put comp :a (scan-number (slice line 12)))

      (= "Register B: " (slice line 0 12))
      (put comp :b (scan-number (slice line 12)))

      (= "Register C: " (slice line 0 12))
      (put comp :c (scan-number (slice line 12)))

      (= "Program: ")
      (put comp :mem (map scan-number (string/split "," (slice line 9))))))
  comp)

(defn get-instruction
  [comp]
  (slice (comp :mem) (comp :pc) (+ 2 (comp :pc))))

(defn literal [comp] (get (comp :mem) (inc (comp :pc))))

(defn combo
  [comp]
  (case (literal comp)
    0 0
    1 1
    2 2
    3 3
    4 (comp :a)
    5 (comp :b)
    6 (comp :c)
    7 (assert false "error: combo operand 7 is reserved")
    (assert false (string "error: combo illegal numerical value "
                          (literal comp)))))

(defn dv
  [comp]
  (brshift (comp :a) (combo comp)))
  # (math/trunc (/ (comp :a) (blshift 1 (combo comp)))))

(defn adv
  [comp]
  (put comp :a (dv comp))
  (+= (comp :pc) 2))

(defn bxl
  [comp]
  (put comp :b (bxor (comp :b) (literal comp)))
  (+= (comp :pc) 2))

(defn bst
  [comp]
  (put comp :b (band (combo comp) 7)) # (% (combo comp) 8))
  (+= (comp :pc) 2))

(defn jnz
  [comp]
  (if (zero? (comp :a))
    (+= (comp :pc) 2)
    (put comp :pc (literal comp))))

(defn bxc
  [comp]
  (put comp :b (bxor (comp :b) (comp :c)))
  (+= (comp :pc) 2))

(defn out
  [comp]
  (array/push (comp :out) (band (combo comp) 7)) # (% (combo comp) 8))
  (+= (comp :pc) 2))

(defn bdv
  [comp]
  (put comp :b (dv comp))
  (+= (comp :pc) 2))

(defn cdv
  [comp]
  (put comp :c (dv comp))
  (+= (comp :pc) 2))

(def ops [adv bxl bst jnz bxc out bdv cdv])

(defn print-program
  [comp]
  (def start-pc (comp :pc))
  (def ops-names ["adv" "bxl" "bst" "jnz" "bxc" "out" "bdv" "cdv"])
  (def operand-types
    [:combo :literal :combo :literal :ignored :combo :combo :combo])
  (def ops-comments
    ["A = A >> %s"
     "B = B ^ %s"
     "B = %s & 7"
     "if (A != 0) goto %s"
     "B = B ^ C"
     "out %s & 7"
     "B = A >> %s"
     "C = A >> %s"
     ])
  (def prog-len (length (comp :mem)))
  (while (< (comp :pc) prog-len)
    (def opcode (get (comp :mem) (comp :pc)))
    (def operand (get (comp :mem) (inc (comp :pc))))
    (def operand-str (case (get operand-types opcode)
                       :literal (string operand)
                       :ignored ""
                       :combo (case operand
                                0 "0"
                                1 "1"
                                2 "2"
                                3 "3"
                                4 "A"
                                5 "B"
                                6 "C"
                                7 "<reserved>"
                                "<error>")))
    (printf (string "%s %s\t# " (ops-comments opcode))
            (ops-names opcode) operand-str operand-str)
    (+= (comp :pc) 2))
  (put comp :pc 0))

(defn run-program
  [comp]
  (def prog-len (length (comp :mem)))
  (while (< (comp :pc) prog-len)
    (def opcode (get (comp :mem) (comp :pc)))
    ((ops opcode) comp)))

(defn quine-so-far?
  [comp]
  (def mem-len (length (comp :mem)))
  (def out-len (length (comp :out)))
  # (printf "qsf? mem-len %d out-len %d" mem-len out-len)
  # (printf "(<= out-len mem-len) => %j" (<= out-len mem-len))
  # (printf "= => %j"    (let [mem-prefix (slice (comp :mem) 0 out-len)]
  #                       (= (tuple ;(comp :out)) mem-prefix)))
  (and
    (<= out-len mem-len)
    (let [mem-prefix (slice (comp :mem) 0 out-len)]
      (= (tuple ;(comp :out)) mem-prefix))))

(defn quine?
  "Return true if comp's program is a quine."
  [comp]
  (def prog-len (length (comp :mem)))
  (while (and (< (comp :pc) prog-len)
              (quine-so-far? comp)) # stop as soon as they don't match
    (def opcode (get (comp :mem) (comp :pc)))
    ((ops opcode) comp))
  (and
    (= (length (comp :out)) prog-len)
    (= (tuple ;(comp :out) (comp :mem)))))

# ================ part 1 ================

(defn part1
  [lines]
  (def comp (read-computer lines))
  # (print-program comp)          #DEBUG
  (run-program comp)
  (string/join (map string (comp :out)) ","))

# ================ part 2 ================

(defn part2
  [lines]
  (def comp (read-computer lines))

  # This works but is too slow for the actual program.

  # (var a 0)
  # (var working (table/clone comp))
  # (put working :a a)
  # (while (and
  #          (not (quine? working))
  #          # stop if we're testing and have gone past the expected value
  #          (or (not (dyn :testing))
  #              (< a (inc (scan-number (dyn :testing-expected-value))))))
  #   (set working (table/clone comp))
  #   (put working :a (+= a 1))
  #   )
  # (dec a))

  (def prog-len (length (comp :mem)))
  (def a 0)
  (each i (range 0 prog-len)
    (set a (find-

  (var a 0)
  (var working (table/clone comp))
  (put working :a a)
  (while (and
           (not (quine? working))
           # stop if we're testing and have gone past the expected value
           (or (not (dyn :testing))
               (< a (inc (scan-number (dyn :testing-expected-value))))))
    (set working (table/clone comp))
    (put working :a (+= a 1))
    )
  (dec a))

  # Ok, I decompiled the test and real programs. This is what they do.

  # (if (dyn :testing)
  #   (do
  #     # adv 3	# A = right-shift A by operand
  #     # out A	# out operand & 7
  #     # jnz 0	# if (A != 0) goto operand
  #     #
  #     # every loop shifts A down by 3 bits then outputs the low three bits of A
  #     (var val 0)
  #     (while (not (empty? (comp :mem)))
  #       (set val (+ (* val 8) (array/pop (comp :mem)))))
  #     (* val 8))
  #   (do
  #     # bst A	# B = A & 7
  #     # bxl 2	# B = B ^ 2
  #     # cdv B	# C = right-shift A by B (0-7 bits)
  #     # bxc 	# B = B ^ C
  #     # bxl 3	# B = B ^ 3
  #     # out B	# out B & 7
  #     # adv 3	# A = right-shift A by 3
  #     # jnz 0	# if (A != 0) goto operand

  #     # so for each loop
  #     #    B = (A & 7) xor 2           # 0 - 7
  #     #    B = B xor (A >> B) xor 3    # 
  #     #    output B & 7
  #     #    A = A >> 3
  #     (print-program comp))))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 17))
