#!/usr/bin/env ruby
#
# Toboggan Trajectory

require_relative '../day'
require_relative '../map'

class Day03 < Day
  def part1
    entries = data_lines(1)
    map = Map.new(data_lines(1), wrap_type = :col)
    puts(num_trees_on_slope(map, 3, 1))
  end

  def part2
    map = Map.new(data_lines(1), wrap_type = :col)
    n = [[1, 1], [3, 1], [5, 1], [7, 1], [1, 2]]
        .map { |right, down| num_trees_on_slope(map, right, down) }
        .reduce(:*)
    puts(n)
  end

  def num_trees_on_slope(map, right, down)
    num_trees = 0
    map.row = map.col = 0
    while map.row < map.height
      num_trees += 1 if map.here == '#'
      map.move_by(down, right)
    end
    num_trees
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2020, 3)
end
