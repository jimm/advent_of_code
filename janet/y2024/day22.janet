#!/usr/bin/env janet
#
# Monkey Market

(import ../running)
(import ../util)

# ================ helpers ================

(util/defmem mix
  [a b]
  # Since a and b might be > 32 bits, we can't use bxor; we have to write
  # one.
  (if (and (int? a) (int? b))
    (bxor a b)
    (do
      (var x a)
      (var y b)
      (var mixed 0)
      (var power 0)
      (while (and (or (not (zero? x)) (not (zero? y))))
                  # (not= x math/nan) (not= y math/nan))
        (let [xlow (% x 2)
              ylow (% y 2)
              new-bit (bxor xlow ylow)]
          (if (= new-bit 1)
            (+= mixed (math/pow 2 power)))
          (+= power 1)
          (set x (math/trunc (/ x 2)))
          (set y (math/trunc (/ y 2)))))
      mixed)))

(util/defmem prune
  [a]
  (% a 16777216))

(util/defmem next-secret
  [secret]
  (var ns secret)
  (set ns (prune (mix ns (* 64 ns))))
  (set ns (prune (mix ns (math/trunc (/ ns 32)))))
  (set ns (prune (mix ns (* 2048 ns)))))

(defn n-gen-secrets
  [n secret]
  (var new-secret secret)
  (repeat 2000
          (set new-secret (next-secret new-secret)))
  new-secret)

# ================ part 1 ================

(defn part1
  [lines]
  (+ ;(map |(n-gen-secrets 2000 (scan-number $)) lines)))

# ================ part 2 ================

(defn part2
  [lines]
  )

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 22))
