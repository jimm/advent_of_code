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

(defn part1 [lines]
  (let [bag-contents {:red 12 :green 13 :blue 14}
        games (read-games lines)]
    (+ ;(map |($ :id)
             (filter |(possible? $ bag-contents) games)))))

# ================ part 2 ================

(defn power-of-minimum-cubes [game]
  (var color-maxes @{:red 0 :green 0 :blue 0})
  (map (fn [color]
         (let [color-max (max-of (map |(or ($ color) 0) (game :draws)))]
           (when (> color-max (color-maxes color))
             (put color-maxes color color-max))))
   [:red :green :blue])
  (* (get color-maxes :red) (get color-maxes :green) (get color-maxes :blue)))

(defn part2 [lines]
  (let [games (read-games lines)]
    (+ ;(map power-of-minimum-cubes games))))

# ================ main ================

(defn main [& args]
  (running/run-main part1 part2 2023 2))
