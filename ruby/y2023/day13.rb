#!/usr/bin/env ruby
#
# Point of Incidence

require_relative '../day'
require_relative '../enumerable'
require_relative '../map'

class Day13 < Day
  def part1(lines = nil)
    lines ||= data_lines(1, false)
    puts do_part1(lines)
  end

  def do_part1(lines)
    map_lines = lines.split(&:empty?)
    map_lines.map do |mlines|
      map = Map.new(mlines)
      reflection_num(map)
    end.sum
  end

  def do_part2(lines)
    # TODO
  end

  private

  def reflection_num(map)
    rows = map.rows.map(&:join)
    n = reflection_point(rows)
    return (n + 1) * 100 if n

    cols = (0..map.height).map { map.column(_1).join }
    n = reflection_point(cols)
    return n + 1 if n

    0
  end

  def reflection_point(nums)
    left = [nums.shift]
    right = nums
    i = 0
    until right.empty?
      min_len = [left.length, right.length].min
      return i if left[-min_len..].reverse == right[..min_len-1]
      left << right.shift
      i += 1
    end
    nil
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
