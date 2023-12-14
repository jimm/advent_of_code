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
      n, _, _ = reflection_num(map)
    end.sum
  end

  def part2(lines = nil)
    lines ||= data_lines(1, false)
    puts do_part2(lines)
  end

  def do_part2(lines)
    map_lines = lines.split(&:empty?)
    map_lines.map do |mlines|
      map = Map.new(mlines)
      old_reflection_num = reflection_num(map)
      smudged_reflection_num(map, old_reflection_num).pdebug("smudged_reflection_num")
    end.sum
  end

  private

  def reflection_num(map)
    rows = map.rows.map(&:join)
    n = reflection_point(rows)
    return (n + 1) * 100 if n

    cols = (0..map.height).map { map.column(_1).join }
    n = reflection_point(cols)
    return n + 1 if n

    nil
  end

  def smudged_reflection_num(map, old_reflection_num)
    warn "---- #{old_reflection_num}" # DEBUG
    map.each do |ri, ci, ch|
      map.set(ri, ci, ch == '#' ? '.' : '#')
      result = reflection_num(map)
      map.set(ri, ci, ch)
      next if result.nil? || !in_reflection?(map, result, ri, ci)
      warn "SMUDGE AT #{ri}, #{ci}" if result && result != old_reflection_num # DEBUG
      return result if result # && result != old_reflection_num
    end
    warn "NOT FOUND" # DEBUG
    old_reflection_num
  end

  # Returns true if (ri, ci) is within the reflection whose reflect_num is n.
  def in_reflection?(map, n, ri, ci)
    warn "in_reflection? n = #{n}, (#{ri}, #{ci})" # DEBUG
    if n >= 100
      warn "row" # DEBUG
      # row
      n = (n / 100) - 1
      # DEBUG n == 0 means rows 0 and 1 are the reflection
      # DEBUG n == 1 means rows 0,1,2,3 are the reflection
      reflection_height = [n, map.height - n].min
      warn "n => #{n}, reflection_height = #{reflection_height}" # DEBUG
      (n-reflection_height..n+reflection_height+1).include?(ri)
    else
      warn "column" # DEBUG
      # column
      n -= 1
      reflection_width = [n, map.width - n].min
      warn "n => #{n}, reflection_width = #{reflection_width}" # DEBUG
      (n-reflection_width..n+reflection_width+1).include?(ci)
    end
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
