#!/usr/bin/env janet
#
# Disk Fragmenter

(import ../running)

# ================ helpers ================

(defn free?
  [block]
  (= :free (block :id)))

(defn read-disk
  [line]
  (def ch-zero (chr "0"))
  (def nums (map |(- $ ch-zero) (string/bytes line)))
  (var disk (seq [[i len] :pairs nums]
                 {:id (if (odd? i) :free (/ i 2))
                  :len len}))
  (if (free? (last disk))
    (array/pop disk))
  disk)

(defn next-free-index
  "Returns index of next free block starting at i (or zero) with length >=
min-length (or 1). Returns nil if not found."
  [disk &opt i min-length]
  (def start (or i 1))
  (def min-len (or min-length 0))
  (def slice-free-idx (find-index
                       (fn [block]
                         (and (free? block) (>= (block :len) min-len)))
                       (slice disk start)))
  (if (nil? slice-free-idx) nil (+ start slice-free-idx)))

(defn defrag-blocks
  [fragged-disk]
  (var disk (array ;fragged-disk)) #make a mutable copy
  (var free-index (next-free-index disk))
  (while free-index
    (var free-block (disk free-index))
    (var end-block (last disk))
    (cond
      # free block is empty
      (= (free-block :len) 0)
      (array/remove disk free-index)
      # free space exceeds end block length
      (> (free-block :len) (end-block :len))
      (do
        (array/insert disk free-index end-block)
        (put disk (inc free-index)
             {:id :free :len (- (free-block :len) (end-block :len))})
        (array/pop disk))
      # free space equals end block length
      (= (free-block :len) (end-block :len))
      (do
        (put disk free-index end-block)
        (array/pop disk))
      # free space less than end block length
      (do
        (put disk free-index
             {:id (end-block :id) :len (free-block :len)})
        (put disk (dec (length disk))
             {:id (end-block :id)
              :len (- (end-block :len) (free-block :len))})))
    # find next free block
    (set free-index (next-free-index disk free-index)))
  disk)

# Let's start this with a naive implementation and see if it's too slow.
(defn defrag-files
  [fragged-disk]
  (var disk (array ;fragged-disk)) #make a mutable copy
  (var block (last disk))
  (var block-index (dec (length disk)))
  (var max-id (block :id))
  (while (> max-id 0)
    (def free-index (next-free-index disk 0 (block :len)))
    # When we find a free block that is before this block,
    # move it
    (when (and free-index (> block-index free-index))
      # We're moving the block so make the existing segments free
      (put disk block-index {:id :free :len (block :len)})
      # Move the file in to the free blcok
      (def free-block (disk free-index))
      (if (= (block :len) (free-block :len))
        # same length
        (put disk free-index block)
        # free block is larger
        (do
          (array/insert disk free-index block)
          (put disk (inc free-index)
               {:id :free :len (- (free-block :len) (block :len))}))))
    # find the next block to move
    (-= max-id 1)
    (set block-index (find-index |(= ($ :id) max-id) disk))
    (set block (disk block-index)))
  disk)

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
  (set disk (defrag-blocks disk))
  (checksum disk))

# ================ part 2 ================

(defn part2 [lines]
  (var disk (read-disk (first lines)))
  (set disk (defrag-files disk))
  (checksum disk))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2024 9))
