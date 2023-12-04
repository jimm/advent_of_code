#!/usr/bin/env ruby
#
# Cube Conundrum

require_relative '../day'

class Day02 < Day
  def do_part1(lines)
    bag_contents = { red: 12, green: 13, blue: 14 }
    games = read_games(lines)
    games.select { |game| possible?(game, bag_contents) }.sum { |game| game[:id] }
  end

  def do_part2(lines)
    games = read_games(lines)
    games.map { power_of_minimum_cubes(_1) }.sum
  end

  private

  def possible?(game, bag_contents)
    game[:draws].all? do |draw|
      draw.keys.all? { draw[_1] <= bag_contents[_1] }
    end
  end

  def power_of_minimum_cubes(game)
    color_maxes = { red: 0, green: 0, blue: 0 }
    %i[red green blue].map do |color|
      color_max = game[:draws].map { _1[color] }.max
      color_maxes[color] = color_max if color_maxes[color].nil? || color_max > color_maxes[color]
    end
    color_maxes[:red] * color_maxes[:green] * color_maxes[:blue]
  end

  def read_games(lines)
    lines.map do |line|
      game_name, data = line.split(': ')
      game = { id: game_name.split(' ')[1].to_i }
      draws = data.split('; ')
      game[:draws] = draws.map do |draw|
        results = Hash.new { |h, k| h[k] = 0 }
        cubes = draw.split(', ')
        cubes.each do |num_and_color|
          num, color = num_and_color.split(' ')
          results[color.to_sym] = num.to_i
        end
        results
      end
      game
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
