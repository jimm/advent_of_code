#!/usr/bin/env janet

# ================ Cube Conundrum ================

(import ../data :as data)
(import ../testing :as testing)
(import ../running :as running)

# ================ part 1 ================

(defn build-draw [draw]
  (var results @{})
  (each cube (string/split ", " draw)
    (do
      (def [num color] (string/split " " cube))
      (set (results (keyword color)) (parse num))))
  results)

(defn build-draws [data]
  (map build-draw (string/split "; " data)))

(defn read-games [lines]
  (map (fn [line]
         (def [game-name data] (string/split ": " line))
         {:id (parse ((string/split " " game-name) 1))
          :draws (build-draws data)})
       lines))

(defn draw-possible? [draw bag-contents]
  (all |(<= (draw $) (bag-contents $))
       (keys draw)))

(defn possible? [game bag-contents]
  (all |(draw-possible? $ bag-contents)
       (game :draws)))

(defn do-part1 [lines]
  (let [bag-contents {:red 12 :green 13 :blue 14}
        games (read-games lines)]
    (+ ;(map |($ :id)
             (filter |(possible? $ bag-contents) games)))))

# ================ part 2 ================

(defn do-part2 [lines]
  )

# ================ main ================

(defn part1 []
  (do-part1 (data/input-lines 2023 2 1)))

(defn test-part1 []
  (testing/run-tests do-part1 2023 2 1))

(defn part2 []
  (do-part2 (data/input-lines 2023 2 2)))

(defn test-part2 []
  (testing/run-tests do-part2 2023 2 2))

(defn main [& args]
  (running/run part1 test-part1 part2 test-part2 ;args))
