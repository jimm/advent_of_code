# Custom Customs

require 'set'
require_relative '../utils'

class Day06 < Day
  def part1
    answer = sum_grouped_data { |group|
      Set.new(group.join('').chars)
    }
    puts(answer)
  end

  def part2
    answer = sum_grouped_data { |group|
      group
        .map { |answers| Set.new(answers.chars) }
        .reduce(:intersection)
    }
    puts(answer)
  end

  # Split input into groups, run the given block on each group (which should
  # return an integer), and add up the answers.
  def sum_grouped_data(&answers)
    data_lines(1, skip_empty_lines=false)
      .split(:empty?)
      .map { |group| answers.call(group).length }
      .reduce(:+)
  end
end
