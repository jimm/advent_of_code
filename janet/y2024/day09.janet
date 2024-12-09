#!/usr/bin/env janet
#
# Disk Fragmenter

(import ../running)

# ================ helpers ================

(defn read-disk
  [line]
  (def ch-zero (chr "0"))
  (def nums (map |(- $ ch-zero) (string/bytes line)))
  (seq [[i len] :pairs nums]
       {:id (if (odd? i) :free (/ i 2))
        :len len}))

(defn free?
  [block]
  (= :free (block :id)))

(defn next-free-index
  [disk &opt i]
  (def start (or i 0))
  (def slice-free-idx (find-index free? (slice disk start)))
  (if (nil? slice-free-idx) nil (+ start slice-free-idx)))

(defn defrag
  [disk]
  (var defragged (array ;disk))
  (if (free? (last defragged)) (array/pop defragged))
  (var free-index (next-free-index defragged))
  (while free-index
    (var free-block (defragged free-index))
    (var end-block (last defragged))
    (cond
      # free space exceeds end block length
      (> (free-block :len) (end-block :len))
      (do
        (array/insert defragged free-index end-block)
        (put defragged (inc free-index)
             {:id :free :len (- (free-block :len) (end-block :len))})
        (array/pop defragged))
      # free space equals end block length
      (= (free-block :len) (end-block :len))
      (do
        (put defragged free-index end-block)
        (array/pop defragged))
      # free space less than end block length
      (do
        (put defragged free-index
             {:id (end-block :id) :len (free-block :len)})
        (put defragged (dec (length defragged))
             {:id (end-block :id)
              :len (- (end-block :len) (free-block :len))})))
    # find next free block
    (set free-index (next-free-index defragged free-index)))
  defragged)

(defn checksum
  [disk]
  (var i 0)
  (var csum 0)
  (each block disk
    (if (free? block)
      (+= i (block :len))
      (repeat (block :len)
              (+= csum (* i (block :id)))
              (+= i 1))))
  csum)

# ================ part 1 ================

(defn part1 [lines]
  (var disk (read-disk (first lines)))
  (set disk (defrag disk))
  (checksum disk))

# ================ part 2 ================

(defn part2 [lines]
  (var disk (read-disk (first lines)))
  (checksum (defrag disk)))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 9))
