#!/usr/bin/env ruby
#
# 1202 Program Alarm

require_relative '../day'
require_relative 'intcode'

class Day02 < Day
  PART2_TARGET_OUTPUT = 19_690_720

  def do_part1(lines)
    lines ||= data_lines(1)
    computer = IntcodeComputer.new
    computer.load_memory(lines)
    unless @testing
      computer.data[1] = 12
      computer.data[2] = 2
    end
    computer.run
    computer.data[0]
  end

  def do_part2(lines)
    lines ||= data_lines(1)
    return no_tests if @testing

    for noun in (0..99)
      for verb in (0..99)
        computer = IntcodeComputer.new
        computer.load_memory(lines)
        computer.data[1] = noun
        computer.data[2] = verb
        computer.run
        return 100 * noun + verb if computer.data[0] == PART2_TARGET_OUTPUT
      end
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
