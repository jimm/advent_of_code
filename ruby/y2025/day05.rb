#!/usr/bin/env ruby
#
# Cafeteria

require_relative '../day'

class Day05 < Day
  def do_part1(lines)
    ranges, ids = _parse(lines)
    fresh = []
    ids.select do |id|
      fresh << id if ranges.detect { |r| r.include?(id) }
    end
    fresh.length
  end

  def do_part2(lines)
    ranges, = _parse(lines)
    # FIXME
    ids = []
    ranges.each { |r| ids |= r.to_a }
    ids.length
  end

  def _parse(lines)
    ranges = []
    ids = []
    lines.each do |line|
      next if line.empty?

      a, b = line.split('-')
      if b.nil? # not a range
        ids << a.to_i
      else
        ranges << (a.to_i..b.to_i)
      end
    end
    [ranges, ids]
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
