#!/usr/bin/env ruby
#
# Custom Customs

require 'set'
require_relative '../day'
require_relative '../enumerable'

class Day06 < Day
  def part1
    answer = sum_grouped_data do |group|
      Set.new(group.join('').chars)
    end
    puts(answer)
  end

  def part2
    answer = sum_grouped_data do |group|
      group
        .map { |answers| Set.new(answers.chars) }
        .reduce(:intersection)
    end
    puts(answer)
  end

  # Split input into groups, run the given block on each group (which should
  # return an integer), and add up the answers.
  def sum_grouped_data(&answers)
    data_lines(1, skip_empty_lines = false)
      .split(:empty?)
      .map { |group| answers.call(group).length }
      .reduce(:+)
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
