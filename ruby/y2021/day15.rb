# Puzzle Name

require 'set'
require_relative '../map'

# All coord pairs are [row, col].
class Day15 < Day
  def part1
    do_part(:build_map)
  end

  def part1_tests
    do_tests(:first, :build_map)
  end

  def part2
    do_part(:build_big_map)
  end

  def part2_tests
    do_tests(:last, :build_big_map)
  end

  def do_tests(expected_extractor, map_builder)
    run_chunk_tests(1) do |expected, lines|
      map = send(map_builder, lines)
      expected = expected.split(',').send(expected_extractor).to_i
      path = a_star(map, [0, 0], [map.height - 1, map.width - 1])
      cost = path.map { |cell| map.at(*cell) }.sum
      [cost == expected, cost]
    end
  end

  def do_part(map_builder)
    map = send(map_builder, data_lines(1))
    path = a_star(map, [0, 0], [map.height - 1, map.width - 1])
    cost = path.map { |cell| map.at(*cell) }.sum
    puts cost
  end

  def build_map(lines)
    Map.new(lines).tap { |m| m.cells_to_ints! }
  end

  def build_big_map(lines)
    new_lines = []
    5.times do |_|
      lines.each do |line|
        str = ''
        5.times { str << line.dup }
        new_lines << str
      end
    end

    tile_height = lines.length
    tile_width = lines[0].length
    map = Map.new(new_lines).tap { |m| m.cells_to_ints! }
    map.height.times do |row|
      map.width.times do |col|
        delta = (row / tile_height) + (col / tile_width)
        new_val = map.at(row, col) + delta
        new_val -= 9 while new_val > 9
        map.set(row, col, new_val)
      end
    end
    map
  end

  def heuristic(map, cell)
    map.at(*cell)
  end

  def a_star(map, start, goal)
    neighbors = [[0, 1], [1, 0], [-1, 0], [0, -1]]
    close_set = Set.new
    came_from = {}
    gscore = Hash.new { |h, k| h[k] = 0 }
    gscore[start] = 0
    fscore = Hash.new { |h, k| h[k] = 0 }
    fscore[start] = heuristic(map, start)
    oheap = []

    oheap.push([fscore[start], start])
    until oheap.empty?
      # find oheap entry with min fscore
      min_fscore = oheap.map(&:first).min
      fscore_and_cell = oheap.detect { |val| val[0] == min_fscore }
      oheap.delete(fscore_and_cell)
      current = fscore_and_cell[1]

      if current == goal
        data = []
        while came_from.include?(current)
          data << current
          current = came_from[current]
        end
        return data
      end

      close_set.add(current)
      neighbors.each do |delta|
        neighbor = [current[0] + delta[0], current[1] + delta[1]]
        next unless map.in_bounds?(*neighbor)

        tentative_g_score = gscore[current] + heuristic(map, neighbor)
        next if close_set.include?(neighbor) && tentative_g_score >= gscore[neighbor]

        next unless tentative_g_score < gscore[neighbor] || !oheap.map(&:last).include?(neighbor)

        came_from[neighbor] = current
        gscore[neighbor] = tentative_g_score
        fscore[neighbor] = tentative_g_score + heuristic(map, neighbor)
        oheap.push([fscore[neighbor], neighbor])
      end
    end
    nil
  end
end
