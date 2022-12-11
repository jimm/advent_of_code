#!/usr/bin/env ruby
#
# Beacon Scanner

require_relative '../day'
require_relative '../point'

class Day19 < Day
  def part1
    lines = data_lines(1)
  end

  def part1_tests
    scanners = parse(data_lines(1))
    scanners.combination(2) do |s0, s1|
      num_in_common = max_in_common(s0, s1)
      puts num_in_common
    end

    # run_chunk_tests(1) do |expected, lines|
    #   scanners = parse(lines)
    #   pp scanners
    # end
  end

  def part2
    lines = data_lines(1)
  end

  def max_in_common(s0, s1)
    n = 0
    (-1..1).each do |rot_x|
      (-1..1).each do |rot_y|
        (-1..1).each do |rot_z|
          # TODO
        end
      end
    end
    rotations.each
  end

  def parse(lines)
    scanners = []
    scanner = nil
    lines.each do |line|
      case line
      when /scanner/
        scanner = []
        scanners << scanner
      when /(-?\d+),(-?\d+),(-?\d+)/
        scanner << Point.new(
          Regexp.last_match(1).to_i,
          Regexp.last_match(2).to_i,
          Regexp.last_match(3).to_i
        )
      else
        raise 'huh?'
      end
    end
    scanners
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc
end
