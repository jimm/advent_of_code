#!/usr/bin/env ruby
#
# Printing Department

require_relative '../day'
require_relative '../map'

class Day04 < Day
  def do_part1(lines)
    map = Map.new(lines)
    num_accessable = 0
    map.each do |row, col, cell|
      num_accessable += 1 if accessable_roll?(map, row, col, cell)
    end
    num_accessable
  end

  def do_part2(lines)
    map = Map.new(lines)
    total_removable = 0
    removable = removables(map)
    until removable.empty?
      remove(map, removable)
      total_removable += removable.length
      removable = removables(map)
    end
    total_removable
  end

  private

  def accessable_roll?(map, row, col, cell)
    cell == '@' && neighbor_count(map, row, col) < 4
  end

  def removables(map)
    removable = []
    map.each do |row, col, cell|
      removable << [row, col] if accessable_roll?(map, row, col, cell)
    end
    removable
  end

  def remove(map, removable)
    removable.each do |row, col|
      map.set(row, col, '.')
    end
  end

  def neighbor_count(map, row, col)
    count = 0
    (row - 1..row + 1).each do |r|
      (col - 1..col + 1).each do |c|
        next if r == row && c == col

        count += 1 if map.at(r, c) == '@'
      end
    end
    count
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
