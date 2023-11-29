#!/usr/bin/env ruby
#
# Sunny with a Chance of Asteroids

require_relative '../day'
require_relative 'intcode'

class Day05 < Day
  def do_part1(lines)
    computer = IntcodeComputer.new(lines)
    computer.inputs = [1]
    computer.run
  end

  def do_part2(lines)
    computer = IntcodeComputer.new(lines)
    computer.inputs = [5]
    computer.run
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2019, 5)
end
