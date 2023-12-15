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
    do_part(lines, 0)
  end

  def part2(lines = nil)
    lines ||= data_lines(1, false)
    puts do_part2(lines)
  end

  def do_part2(lines)
    do_part(lines, 1)
  end

  def do_part(lines, num_smudges_allowed)
    map_lines = lines.split(&:empty?)
    map_lines.map do |mlines|
      map = Map.new(mlines)
      n, _, _ = reflection_num(map, num_smudges_allowed)
    end.sum
  end

  private

  def reflection_num(map, num_smudges_allowed)
    rows = map.rows.map(&:join)
    n = reflection_point(rows, num_smudges_allowed)
    return (n + 1) * 100 if n

    cols = map.columns.map(&:join)
    n = reflection_point(cols, num_smudges_allowed)
    return n + 1 if n

    nil
  end

  def reflection_point(nums, num_smudges_allowed)
    left = [nums.shift]
    right = nums
    i = 0
    until right.empty?
      min_len = [left.length, right.length].min
      num_diffs = compare_image_and_reflection(left[-min_len..].reverse, right[..min_len-1])
      return i if num_diffs == num_smudges_allowed
      left << right.shift
      i += 1
    end
    nil
  end

  # Returns the number of differences between the two sides.
  def compare_image_and_reflection(side0, side1)
    side0.zip(side1).sum do |s0, s1|
      s0.chars.zip(s1.chars).sum do |ch0, ch1|
        ch0 == ch1 ? 0 : 1
      end
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
