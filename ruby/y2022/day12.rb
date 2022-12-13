#!/usr/bin/env ruby
#
# Hill Climbing Algorithm

require 'set'
require_relative '../day'
require_relative '../map'

class Day12 < Day
  def part1
    puts do_part1(data_lines(1))
  end

  def part1_tests
    do_tests
  end

  def part2
    puts do_part2(data_lines(1))
  end

  def part2_tests
    do_tests
  end

  private

  def do_tests
    run_chunk_tests(1) do |expected, lines|
      expected = expected.split(',')[@part_number - 1].to_i
      answer = send("do_part#{@part_number}".to_sym, lines)
      [answer == expected, answer, expected]
    end
  end

  def do_part1(lines)
    map, start_loc, end_loc = map_from(lines)
    a_star(map, start_loc, end_loc).length
  end

  # Can't brute force this. Besides, looking at the map shows that there is
  # an "island" outside of which we can't even get to the goal, and that
  # outside area has tons of potential starting positions we need to ignore.
  def do_part2(lines)
    map, _, end_loc = map_from(lines)
    remove_inaccessible_cells(map, end_loc)
    lengths = []
    # FIXME
    map.height.times do |row|
      map.width.times do |col|
        lengths << a_star(map, [row, col], end_loc) if map.at(row, col) == 0
      end
    end
    lengths.compact.map(&:length).min
  end

  def map_from(lines)
    start_loc = end_loc = nil
    map = Map.new(lines)
    map.height.times do |row|
      map.width.times do |col|
        ch = map.at(row, col)
        case ch
        when ('a'..'z')
          map.set(row, col, ch.ord - 'a'.ord)
        when 'S'
          map.set(row, col, 0)
          start_loc = [row, col]
        when 'E'
          map.set(row, col, 'z'.ord - 'a'.ord)
          end_loc = [row, col]
        end
      end
    end
    [map, start_loc, end_loc]
  end

  # if curr_loc elevation is higher than current elevation, return very high
  # value. Else return low value.
  def heuristic(map, there, here)
    there_val = map.at(*there)
    here_val = map.at(*here)
    there_val > (here_val + 1) ? 10_000 : 1
  end

  def a_star(map, start, goal)
    neighbors = [[0, 1], [1, 0], [-1, 0], [0, -1]]
    close_set = Set.new
    came_from = {}
    gscore = Hash.new { |h, k| h[k] = 0 }
    gscore[start] = 0
    fscore = Hash.new { |h, k| h[k] = 0 }
    fscore[start] = heuristic(map, start, start)
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
        return data.reverse
      end

      close_set.add(current)
      neighbors.each do |delta|
        neighbor = [current[0] + delta[0], current[1] + delta[1]]
        next unless map.in_bounds?(*neighbor)

        tentative_g_score = gscore[current] + heuristic(map, neighbor, current)
        next if close_set.include?(neighbor) && tentative_g_score >= gscore[neighbor]

        next unless tentative_g_score < gscore[neighbor] || !oheap.map(&:last).include?(neighbor)

        came_from[neighbor] = current
        gscore[neighbor] = tentative_g_score
        fscore[neighbor] = tentative_g_score + heuristic(map, neighbor, current)
        oheap.push([fscore[neighbor], neighbor])
      end
    end
    nil
  end

  # Given a `map` and the `goal` cell coordinates, removes all cells from
  # `map` that are inaccessable from the goal. Removal in this case is
  # defined by setting the value to -1, because all we really care about is
  # removing any zero-valued cells that we can ignore as starting points.
  def remove_inaccessible_cells(map, goal)
    # Use a flood fill to keep cells; everything not flooded gets a -1.
    basin = Set.new
    flood_fill(map, basin, goal[0], goal[1])
    map.height.times do |row|
      map.width.times do |col|
        map.set(row, col, '.') unless basin.include?([row, col])
      end
    end

    # DEBUG
    map.height.times do |row|
      map.width.times do |col|
        val = map.at(row, col)
        next if val == '.'

        map.set(row, col, (val + 'a'.ord).chr)
        map.set(row, col, '.') unless basin.include?([row, col])
      end
    end

    # DEBUG
    puts map # DEBUG

    # DEBUG
    map.height.times do |row|
      map.width.times do |col|
        map.set(row, col, map.at(row, col).ord - 'a'.ord)
      end
    end
    exit 0                      # DEBUG
  end

  def flood_fill(map, basin, row, col)
    return unless map.in_bounds?(row, col)
    return if basin.include?([row, col])

    val = map.at(row, col)
    return if val.nil? || val == 9

    basin.add([row, col])

    [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |dr, dc|
      r = row + dr
      c = col + dc
      other_val = map.at(r, c)
      flood_fill(map, basin, r, c) if other_val && other_val >= (val - 1)
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2022, 12)
end
