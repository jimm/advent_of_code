#!/usr/bin/env ruby
#
# Report Repair

require_relative '../day'

class Day01 < Day
  def part1
    puts(product_of_2020_sum(2))
  end

  def part2
    puts(product_of_2020_sum(3))
  end

  def product_of_2020_sum(n)
    data_lines(1)
      .map(&:to_i)
      .combination(n)
      .detect { |vals| vals.sum == 2020 }
      .reduce(:*)
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc
end
