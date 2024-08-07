# ================ running ================

(defn run [p1 tp1 p2 tp2]
  (def testing (= ((dyn :args) 1) "-t"))
  (def part (parse ((dyn :args) (if testing 2 1))))
  (when (= part 1)
    (if testing (tp1) (print (p1))))
  (when (= part 2)
    (if testing (tp2) (print (p2)))))
