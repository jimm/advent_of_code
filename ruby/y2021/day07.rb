#!/usr/bin/env ruby
#
# The Treachery of Whales

require_relative '../day'

class Day07 < Day
  def part1
    positions = data_lines(1).first.split(',').map(&:to_i)
    min_used = min_fuel(positions) { |distance| distance }
    puts min_used
  end

  def part2
    positions = data_lines(1).first.split(',').map(&:to_i)
    min_used = min_fuel(positions) do |distance|
      (distance * distance + distance) / 2
    end
    puts min_used
  end

  def min_fuel(positions, &block)
    min = positions.min
    max = positions.max
    used = []
    min.upto(max) do |pos|
      used << fuel_used(positions, pos, block)
    end
    used.min
  end

  def fuel_used(positions, target, block)
    positions.reduce(0) do |acc, pos|
      distance = (target - pos).abs
      acc + block.call(distance)
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2021, 7)
end
