# Toboggan Trajectory

require_relative '../map'

class Day03 < Day
  def part1
    entries = data_lines(1)
    map = Map.new(data_lines(1))
    puts(num_trees_on_slope(map, 3, 1))
  end

  def part2
    map = Map.new(data_lines(1))
    n = num_trees_on_slope(map, 1, 1) *
        num_trees_on_slope(map, 3, 1) *
        num_trees_on_slope(map, 5, 1) *
        num_trees_on_slope(map, 7, 1) *
        num_trees_on_slope(map, 1, 2)
    puts(n)
  end

  def num_trees_on_slope(map, right, down)
    num_trees = 0
    row_idx = 0
    col_idx = 0
    while row_idx < map.height
      num_trees += 1 if map.at(row_idx, col_idx % map.width, wrap: true) == '#'
      row_idx += down
      col_idx += right
    end
    num_trees
  end
end
