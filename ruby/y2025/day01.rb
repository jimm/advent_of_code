#!/usr/bin/env ruby
#
# Secret Entrance

require_relative '../day'

class Day01 < Day
  def do_part1(lines)
    pos = 50
    lines.reduce(0) do |num_zeroes, line|
      pos, = _move(pos, line)
      num_zeroes + (pos == 0 ? 1 : 0)
    end
  end

  def do_part2(lines)
    pos = 50
    lines.reduce(0) do |num_zeroes, line|
      pos, num_zeroes_seen = _move(pos, line)
      num_zeroes + num_zeroes_seen
    end
  end

  def _move(pos, line)
    direction = line[0]
    num = line[1..].to_i
    delta = direction == 'R' ? 1 : -1
    num_zeroes = 0
    num.times do |_|
      pos = (pos + delta) % 100
      num_zeroes += 1 if pos == 0
    end
    pos %= 100
    [pos, num_zeroes]
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
