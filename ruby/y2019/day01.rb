#!/usr/bin/env ruby
#
# The Tyranny of the Rocket Equation

require_relative '../day'

class Day01 < Day
  def do_part1(lines)
    lines.inject(0) { |acc, line| acc += fuel_for_mass(line.to_i) }
  end

  def do_part2(lines)
    lines.inject(0) { |acc, line| acc += fuel_for_mass_including_additional(line.to_i) }
  end

  private

  def fuel_for_mass(mass)
    mass / 3 - 2
  end

  def fuel_for_mass_including_additional(mass)
    sum = 0
    answer = mass / 3 - 2
    while answer > 0
      sum += answer
      answer = answer / 3 - 2
    end
    sum
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
