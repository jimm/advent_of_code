#!/usr/bin/env ruby
#
# Hot Springs

require_relative '../day'

class Day12 < Day
  def do_part1(lines)
    lines.map do |line|
      springs, nums = line.split
      springs = springs.chars
      nums = nums.split(',').map(&:to_i)

      fit_count(springs, nums)
    end.sum
  end

  def do_part2(lines)
    # TODO
  end

  private

  def fit_count(springs, nums)
    debug("**** fit_count springs = #{springs}")
    unknown_indexes = springs.map.with_index { _1 == '?' ? _2 : nil }.compact
    do_fit_count(springs, nums, unknown_indexes)
  end

  def do_fit_count(springs, nums, unknown_indexes)
    debug("do_fit_count springs = #{springs}, unknown_indexes = #{unknown_indexes}")
    if unknown_indexes.empty?
      return fit_criteria?(springs, nums) ? 1 : 0
    end

    i = unknown_indexes[0]
    ['#', '.'].map do |ch|
      springs[i] = ch
      do_fit_count(springs, nums, unknown_indexes[1..])
    end.sum
  end

  def fit_criteria?(springs, nums)
    springs.join.split('.').reject(&:empty?).map(&:length) == nums
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
