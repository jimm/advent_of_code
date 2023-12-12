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

      springs = (springs.chars << '?') * 5
      nums *= 5

      fit_count(springs, nums)
    end.sum
  end

  private

  def fit_count(springs, nums)
    unknown_indexes = springs.map.with_index { _1 == '?' ? _2 : nil }.compact
    num_known = springs.select { _1 == '#' }.count
    max_num = nums.sum
    do_fit_count(springs, nums, unknown_indexes, num_known, max_num)
  end

  def do_fit_count(springs, nums, unknown_indexes, num_known, max_num)
    return 0 if num_known > max_num
    if unknown_indexes.empty?
      return fit_criteria?(springs, nums) ? 1 : 0
    end

    i = unknown_indexes[0]
    ['#', '.'].map do |ch|
      springs[i] = ch
      do_fit_count(springs, nums, unknown_indexes[1..], num_known + (ch == '#' ? 1 : 0), max_num)
    end.sum
  end

  def fit_criteria?(springs, nums)
    # Easier to code but much slower
    # return springs.chunk { _1 == '#' }.select { _1[0] == true }.map { _1[1].length } == nums

    run_len = 0
    nums_idx = 0
    curr_num = nums[0]
    prev_ch = nil
    springs.each do |ch|
      case ch
      when '.'
        if prev_ch == '#'
          return false if run_len != curr_num

          run_len = 0
          nums_idx += 1
          curr_num = nums[nums_idx]
        end
      when '#'
        run_len += 1
        return false if nums_idx >= nums.length || run_len > curr_num
      end
      prev_ch = ch
    end

    case prev_ch
    when '.'
      nums_idx == nums.length
    when '#'
      nums_idx == nums.length - 1 && run_len == curr_num
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
