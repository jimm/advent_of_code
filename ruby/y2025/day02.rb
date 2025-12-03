#!/usr/bin/env ruby
#
# Gift Shop

require_relative '../day'

class Day02 < Day
  def do_part1(lines)
    _part_common(lines, :_invalid1)
  end

  def do_part2(lines)
    _part_common(lines, :_invalid2)
  end

  def _invalid1(n)
    s = n.to_s
    l = s.length
    return false if l.odd?

    half = l / 2
    s[..half - 1] == s[half..]
  end

  def _invalid2(n)
    s = n.to_s
    l = s.length
    half = l / 2
    (1..half).each do |i|
      # quick validity checks: first or last letters of substring don't
      # match the last possible match
      next if s[0] != s[-i] or s[i - 1] != s[-1]

      pieces = s.chars.each_slice(i).to_a
      substr = pieces[0]
      return true if pieces[1..].all? { |piece| substr == piece }
    end
    false
  end

  def _part_common(lines, invalid_func)
    range_strs = lines[0].split(',')
    ranges = range_strs.map do |rstr|
      mins, maxs = rstr.split('-')
      (mins.to_i..maxs.to_i)
    end
    sum_invalid = 0
    ranges.each do |r|
      r.each do |i|
        sum_invalid += i if send(invalid_func, i)
      end
    end
    sum_invalid
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
