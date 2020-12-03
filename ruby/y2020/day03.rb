# Toboggan Trajectory

class Day03 < Day
  def part1
    entries = data_lines(1)
    map = build_map(entries)
    puts(num_trees_on_slope(map, 3, 1))
  end

  def part2
    entries = data_lines(1)
    map = build_map(entries)
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
    height = map.length
    width = map[0].length
    while row_idx < height
      num_trees += 1 if map[row_idx][col_idx % width] == '#'
      row_idx += down
      col_idx += right
    end
    num_trees
  end

  def build_map(entries)
    entries.map { |line| line.split('') }
  end
end
