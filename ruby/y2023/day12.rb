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
    lines.map do |line|
      springs, nums = line.split
      springs = springs
      nums = nums.split(',').map(&:to_i)

      springs = ((springs.chars << '?') * 5)[..-2] # remove the last '?'
      nums *= 5

      fit_count(springs, nums)
    end.sum
  end

  private

  def fit_count(springs, nums)
    @cache = {}
    do_fit_count(springs, nums)
  end

  def do_fit_count(springs, nums)
    cached_value = @cache[[springs, nums]]
    return cached_value if cached_value

    retval = calc_fit_count(springs, nums)
    @cache[[springs, nums]] = retval
    retval
  end

  def calc_fit_count(springs, nums)
    # if there are no more runs of springs to look for, make sure we have no
    # more springs to find
    if nums.empty?
      return springs.any? { _1 == '#' } ? 0 : 1
    end

    # there are still runs of springs to look for, so if we're out of input
    # then this is not a match
    return 0 if springs.empty?

    case springs[0]
    when '#'
      runlen = nums[0]

      # must have runlen springs or possible springs
      checking = springs.take(runlen)
      return 0 if checking.length < runlen || checking.any? { _1 == '.' }

      # if we're looking at the last group of springs, we know if we're done
      # or not
      if springs.length == runlen
        return nums.length == 1 ? 1 : 0
      end

      # next char must be a possible separator
      return 0 if springs[runlen] == '#'

      # skip that separator and continue
      do_fit_count(springs.drop(runlen + 1), nums.drop(1))
    when '.'
      do_fit_count(springs.drop(1), nums)
    when '?'
      remainder = springs.drop(1)
      do_fit_count(['#'] + remainder, nums) +
        do_fit_count(['.'] + remainder, nums)
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
