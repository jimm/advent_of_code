#!/usr/bin/env ruby
#
# Rambunctious Recitation

require_relative '../day'

class Day15 < Day
  def part1
    do_part(2020)
  end

  def part2
    do_part(30_000_000)
  end

  def do_part(max_turn)
    starting_nums = @testing ? [0, 3, 6] : [0, 1, 4, 13, 15, 12, 16]
    memory = Hash.new { |h, k| h[k] = Array.new(2) }
    starting_nums.each_with_index { |n, i| memory[n] = [nil, i + 1] }
    turn = starting_nums.length + 1
    n = starting_nums.last
    while turn <= max_turn
      n = memory[n][0].nil? ? 0 : memory[n][1] - memory[n][0]
      memory[n][0] = memory[n][1]
      memory[n][1] = turn
      turn += 1
    end
    puts(n)
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2020, 15)
end
