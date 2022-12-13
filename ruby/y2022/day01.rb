#!/usr/bin/env ruby
#
# Calorie Counting

require_relative '../day'

class Day01 < Day
  def part1
    sums = calorie_sums
    puts sums.max
  end

  def part2
    sums = calorie_sums

    max = sums.max
    top_three = max
    sums -= [max]

    max = sums.max
    top_three += max
    sums -= [max]

    max = sums.max
    top_three += max

    puts top_three
  end

  def calorie_sums
    lines = data_lines(1, false)
    groups = lines.slice_when { |line| line.empty? }
    groups.map { |lines| lines.map(&:to_i).sum }
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2022, 1)
end
