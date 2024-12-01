#!/usr/bin/env ruby

# ================ Historian Hysteria ================

require_relative '../day'

class Day01 < Day
  def do_part1(lines)
    col1, col2 = read_columns(lines)
    col1.sort!
    col2.sort!
    col1.zip(col2).sum { |pair| (pair[0] - pair[1]).abs }
  end

  def do_part2(lines)
    col1, col2 = read_columns(lines)
    freqs = col2.tally
    col1.sum { |val| val * (freqs[val] || 0) }
  end

  def read_columns(lines)
    col1 = []
    col2 = []
    lines.each do |line|
      a, b = line.split.map(&:to_i)
      col1 << a
      col2 << b
    end
    [col1, col2]
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
