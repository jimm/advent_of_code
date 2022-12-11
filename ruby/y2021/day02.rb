#!/usr/bin/env ruby
#
# Dive!

require_relative '../day'

class Day02 < Day
  def part1
    distance = depth = 0
    lines = data_lines(1)
    lines.each do |line|
      command, dist = line.split
      dist = dist.to_i
      case command
      when 'forward'
        distance += dist
      when 'down'
        depth += dist
      when 'up'
        depth -= dist
      end
    end
    puts distance * depth
  end

  def part2
    lines = data_lines(1)
    distance = depth = aim = 0
    lines = data_lines(1)
    lines.each do |line|
      command, dist = line.split
      dist = dist.to_i
      case command
      when 'forward'
        distance += dist
        depth += aim * dist
      when 'down'
        aim += dist
      when 'up'
        aim -= dist
      end
    end
    puts distance * depth
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc
end
