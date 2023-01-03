#!/usr/bin/env ruby
#
# Blizzard Basin

require 'set'
require_relative '../day'
require_relative '../map'

class Day24 < Day
  NORTH = 1
  SOUTH = 2
  EAST = 4
  WEST = 8

  def part1
    puts do_part(data_lines(1))
  end

  def part1_tests
    puts do_part(data_lines(1))
  end

  def part2
    puts do_part(data_lines(1))
  end

  def part2_tests
    do_tests
  end

  private

  def do_part(lines)
    val_to_bit = { '^' => NORTH, 'v' => SOUTH, '>' => EAST, '<' => WEST }
    map = Map.new(lines)
    winds = Set.new
    map.each do |ir, ic, val|
      new_val = val_to_bit[val] || val
      winds << [ir, ic, new_val] if [NORTH, SOUTH, EAST, WEST].include?(new_val)
      new_val
    end
    'TODO'
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2022, 24)
end
