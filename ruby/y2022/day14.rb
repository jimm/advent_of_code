#!/usr/bin/env ruby
#
# Regolith Reservoir

require 'set'
require_relative '../day'

class Day14 < Day
  def part1
    puts do_part1(data_lines(1))
  end

  def part1_tests
    do_tests
  end

  def part2
    puts do_part2(data_lines(1))
  end

  def part2_tests
    do_tests
  end

  private

  def do_tests
    run_chunk_tests(1) do |expected, lines|
      expected = expected.split(',')[@part_number - 1].to_i
      answer = send(:"do_part#{@part_number}", lines)
      [answer == expected, answer, expected]
    end
  end

  def do_part1(lines)
    start_loc = [500, 0]
    obstructions = parse(lines)
    max_y = obstructions.to_a.map { |a| a[1] }.max

    grain = 0
    while true
      loc = start_loc.dup
      break unless drop(start_loc, obstructions, max_y)

      grain += 1
    end
    grain
  end

  def do_part2(lines)
    start_loc = [500, 0]
    obstructions = parse(lines)
    min_x, max_x = obstructions.to_a.map { |a| a[0] }.minmax
    max_y = obstructions.to_a.map { |a| a[1] }.max
    # the floor
    (min_x - max_y - 4..max_x + max_y + 4).each { |x| obstructions.add([x, max_y + 2]) }

    grain = 0
    while true
      loc = start_loc.dup
      drop(start_loc, obstructions, max_y + 3) # floor at + 2, avoid fall-through logic
      grain += 1
      break if obstructions.include?([500, 0])
    end
    grain
  end

  # Drops grain of sand at `loc` down. Returns true if the grain stopped,
  # also updating obstructions. Else returns false when it falls past the
  # lowest obstruction.
  def drop(start, obstructions, max_y)
    loc = start.dup
    return false if loc[1] > max_y

    while true
      return false if loc[1] > max_y

      try = [loc[0], loc[1] + 1]
      unless obstructions.include?(try)
        loc = try
        next
      end

      try[0] -= 1
      unless obstructions.include?(try)
        loc = try
        next
      end

      try[0] += 2
      unless obstructions.include?(try)
        loc = try
        next
      end

      obstructions.add(loc)
      return true
    end
    false
  end

  def parse(lines)
    obstructions = Set.new
    lines.each do |line|
      points = line.split(' -> ').map { |str| str.split(',').map(&:to_i) }
      points.each_cons(2) do |a, b|
        if a[0] == b[0]
          min, max = [a[1], b[1]].minmax
          (min..max).each { |y| obstructions.add([a[0], y]) }
        else
          min, max = [a[0], b[0]].minmax
          (min..max).each { |x| obstructions.add([x, a[1]]) }
        end
      end
    end
    obstructions
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2022, 14)
end
