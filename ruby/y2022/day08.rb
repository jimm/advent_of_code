#!/usr/bin/env ruby
#
# Treetop Tree House

require 'set'
require_relative '../day'
require_relative '../map'

class Day08 < Day
  def part1
    puts do_part2(data_lines(1))
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
    map = Map.new(lines)
    map.cells_to_ints!
    visible = Set.new
    (0...map.height).each do |row|
      (0...map.width).each do |col|
        visible.add([row, col]) if visible?(map, row, col)
      end
    end
    visible.size
  end

  def do_part2(lines)
    map = Map.new(lines)
    map.cells_to_ints!

    scenic_scores = []
    map.each do |row, col, val|
      scenic_scores << scenic_score(map, row, col, val)
    end
    scenic_scores.max
  end

  def visible?(map, row, col)
    return true if row == 0 || col == 0 || row == map.height - 1 || col == map.width - 1

    val = map.at(row, col)

    map_row = map.row(row)
    return true if map_row[0..col].max == val && map_row[0..col - 1].max < val
    return true if map_row[col..].max == val && map_row[col + 1..].max < val

    map_col = map.col(col)
    return true if map_col[0..row].max == val && map_col[0..row - 1].max < val
    return true if map_col[row..].max == val && map_col[row + 1..].max < val

    false
  end

  def scenic_score(map, row, col, val)
    distances = []

    dist = 0
    (row - 1).downto(0).each do |ir|
      dist += 1
      break if map.at(ir, col) >= val
    end
    distances << dist

    dist = 0
    (row + 1).upto(map.height - 1).each do |ir|
      dist += 1
      break if map.at(ir, col) >= val
    end
    distances << dist

    dist = 0
    (col - 1).downto(0).each do |ic|
      dist += 1
      break if map.at(row, ic) >= val
    end
    distances << dist

    dist = 0
    (col + 1).upto(map.width - 1).each do |ic|
      dist += 1
      break if map.at(row, ic) >= val
    end
    distances << dist

    distances.reduce(&:*)
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2022, 8)
end
