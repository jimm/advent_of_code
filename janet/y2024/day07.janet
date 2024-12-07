#!/usr/bin/env janet
#
# Bridge Repair

(import ../running)
(import ../util)

# ================ helpers ================

(defn read-equations
  "Equations are tuples of the form [test-val numbers]."
  [lines]
  (map (fn [line]
         (def words (map util/stoi (util/words (string/replace ":" " " line))))
         (tuple (first words) (drop 1 words)))
         lines))

(defn loop-over-ops
  "Calls f for each operator, short-circuiting and returning test-val when
the first one does."
  [f test-val nums operators]
  (var found-val nil)
  (loop [op :in operators :until found-val]
    (set found-val (f test-val nums op operators)))
  found-val)

(defn do-find-solution
  [test-val nums oper operators]
  (cond (empty? nums) false
        (= (length nums) 1) (= (first nums) test-val)
        # else 
        (let [val (oper (first nums) (first (drop 1 nums)))
              new-nums [val ;(drop 2 nums)]]
          (loop-over-ops do-find-solution test-val new-nums operators))))

(defn find-solution
  "Returns test-val when the first solution is found."
  [test-val nums operators]
  (loop-over-ops do-find-solution test-val nums operators))

(defn solvable?
  [eqn operators]
  (find-solution ;eqn operators))

(defn int-concat
  [a b]
  (util/stoi (string a b)))

# ================ part 1 ================

(defn part1 [lines]
  (def equations (read-equations lines))
  (+ ;(map first (filter |(solvable? $ [+ *])
                         equations))))

# ================ part 2 ================

(defn part2 [lines]
  (def equations (read-equations lines))
  (+ ;(map first (filter |(solvable? $ [+ * int-concat])
                         equations))))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 7))
