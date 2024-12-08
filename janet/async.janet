(defn async
  "Spawns a call to f with args and returns a channel."
  [f & args]
  (def chan (ev/thread-chan))
  (ev/spawn-thread (ev/give chan (f ;args)))
  chan)

(defn async-with-chan
  "Spawns a call to f with args that uses chan."
  [chan f & args]
  (ev/spawn-thread (ev/give chan (f ;args))))

(defn await
  "Awaits the first value sent to chan and returns it."
  [chan]
  (ev/take chan))

(defn gather
  "Awaits the first value sent to each channel and returns the array."
  [chans]
  (map ev/take chans))

# ================ example ================
#
# (defn ttest
#   [i]
#   (ev/sleep (* 3 (math/random)))
#   i)
#
# (var thread-chans (map |(async ttest $) (range 0 5)))
# (var results (await thread-chans))
# (pp results)
