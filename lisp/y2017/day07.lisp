;;; Recursive Circus

(defstruct circus-node name weight subtree-weight parent children)

(defun remove-trailing-comma (s)
  (if (equal (subseq s (- (length s) 1)) ",")
      (subseq s 0 (- (length s) 1))
      s))

;; Returns a new circus-node where parent and subtree-weights are nil for
;; now and where children is a list of child names, not nodes. Those will be
;; filled in later.
(defun parse-raw-node (line)
  (let ((words (split-by-space line)))
    (make-circus-node
     :name (first words)
     :weight (to-integer (subseq (second words) 1 (- (length (second words)) 1)))
     :parent nil
     :children (if (> (length words) 3)
                   (mapcar (lambda (s) (remove-trailing-comma s)) (subseq words 3))
                   ()))))

;; Given a circus-node with names for parent and children, link it to its
;; parent and children circus-nodes.
(defun link (cn node-map)
  (setf (circus-node-children cn)
        (mapcar (lambda (name) (gethash name node-map))
                (circus-node-children cn)))
  (mapc (lambda (child) (setf (circus-node-parent child) cn))
        (circus-node-children cn)))

;; Return root circus-node.
(defun read-tree (lines)
  (let* ((raw-nodes (mapcar #'parse-raw-node lines))
         (node-names (mapcar #'circus-node-name raw-nodes))
         (node-map (make-hash-table :test #'equalp)))
    (mapc (lambda (rn) (setf (gethash (circus-node-name rn) node-map) rn))
          raw-nodes)
    (mapc (lambda (name) (link (gethash name node-map) node-map))
          node-names)
    ;; Find name of node with no parent, then look it up in node-map and
    ;; return it.
    (gethash (find-if (lambda (name) (not (circus-node-parent
                                           (gethash name node-map))))
                      node-names)
             node-map)))

(defun test-tree ()
  (read-tree (list
              "pbga (66)"
              "xhth (57)"
              "ebii (61)"
              "havc (66)"
              "ktlj (57)"
              "fwft (72) -> ktlj, cntj, xhth"
              "qoyq (66)"
              "padx (45) -> pbga, havc, qoyq"
              "tknk (41) -> ugml, padx, fwft"
              "jptl (61)"
              "ugml (68) -> gyxo, ebii, jptl"
              "gyxo (61)"
              "cntj (57)")))

;; ================ part 2 ================

(defun calc-subtree-weight (node)
  (apply #'+ (cons (circus-node-weight node)
                   (mapcar #'calc-subtree-weight (circus-node-children node)))))

;; This could be written more efficiently by starting at the root nodes and
;; bubbling the weights up, but the tree is not large enough for this to
;; matter.
(defun set-subtree-weights (node)
  (setf (circus-node-subtree-weight node) (calc-subtree-weight node))
  (mapc #'set-subtree-weights (circus-node-children node))
  nil)

(defun child-subtree-weights (node)
  (mapcar #'circus-node-subtree-weight (circus-node-children node)))

(defun different-element (xs)
  (let ((sorted-xs (sort xs #'<)))
    (if (= (first sorted-xs) (second sorted-xs))
        (first (reverse sorted-xs))
        (first sorted-xs))))

(defun find-misfit (root)
  (let ((child-weights (child-subtree-weights root)))
    ;; Don't have to worry about empty child-weights; we will find the
    ;; misfit before reaching a leaf node.
    (if (apply #'= child-weights)
        root
        (let ((bad-weight (different-element child-weights)))
          (find-misfit
           (find-if (lambda (ch) (= (circus-node-subtree-weight ch) bad-weight))
                    (circus-node-children root)))))))

(defun any-sibling (node)
  (find-if (lambda (n) (not (eql n node)))
           (circus-node-children (circus-node-parent node))))

(defun correct-weight-for (node)
  (let* ((ideal-weight (circus-node-subtree-weight (any-sibling node)))
         (sum-child-weights (apply #'+ (child-subtree-weights node))))
    (- ideal-weight sum-child-weights)))


;; ================ main ================

(defun part1 (&optional test-root)
  (circus-node-name (or test-root (read-tree (input-lines 2017 7)))))

(defun test-part1 () (part1 (test-tree)))

(defun part2 (&optional test-root)
  (let ((root (or test-root (read-tree (input-lines 2017 7)))))
    (set-subtree-weights root)
    (let ((misfit (find-misfit root)))
      (correct-weight-for misfit))))

(defun test-part2 () (part2 (test-tree)))
