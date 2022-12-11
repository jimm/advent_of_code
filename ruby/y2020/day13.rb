#!/usr/bin/env ruby
#
# Shuttle Search

require 'set'
require_relative '../day'

class Day13 < Day
  PART_TWO_HINT_MIN = 100_000_000_000_000

  def part1
    earliest_departure, buses = parse_input
    buses.compact!
    t = earliest_departure
    while true
      buses.each do |bus|
        if t % bus == 0
          puts((t - earliest_departure) * bus)
          exit(0)
        end
      end
      t += 1
    end
  end

  def part2
    _, buses = parse_input
    answer = do_part2(buses, PART_TWO_HINT_MIN)
    puts(answer)
  end

  def part2_tests
    run_chunk_tests do |expected, lines|
      buses = parse_buses(lines[0])
      answer = do_part2(buses)
      [expected.to_i == answer, answer]
    end
  end

  # We look for the first matching bus with time increments of 1. After
  # that's found, we can increment time by that bus number. Each time we
  # find another bus that matches, we increment by the lowest common
  # multiple (lcm) of all buses found.
  def do_part2(buses, min_t = 0)
    bus_indexes = {}
    buses.each_with_index do |bus, i|
      bus_indexes[bus] = i if bus
    end

    t = min_t
    increment = 1
    buses_found = []
    buses = bus_indexes.keys
    while true
      buses.each do |bus|
        if (t + bus_indexes[bus]) % bus == 0
          buses_found << bus
          increment = buses_found.reduce(:lcm)
        end
      end
      buses -= buses_found
      return t if buses.empty?

      t += increment
    end
  end

  def parse_input
    lines = data_lines(1)
    [lines[0].to_i, parse_buses(lines[1])]
  end

  def parse_buses(line)
    line
      .split(',')
      .map { |val| val == 'x' ? nil : val.to_i }
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc
end
