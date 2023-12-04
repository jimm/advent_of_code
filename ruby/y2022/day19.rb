#!/usr/bin/env ruby
#
# Not Enough Minerals

require_relative '../day'

class Day19 < Day
  def part1
    puts do_part(data_lines(1))
  end

  def part1_tests
    do_tests
  end

  def part2
    puts do_part(data_lines(1))
  end

  def part2_tests
    do_tests
  end

  private

  def do_tests
    run_chunk_tests(1) do |expected, lines|
      expected = expected.split(',')[@part_number - 1].to_i
      answer = do_part(lines)
      [answer == expected, answer, expected]
    end
  end

  def do_part(lines)
    blueprints = parse(lines)
  end

  def parse(lines)
    lines.map do |line|
      line =~ /Blueprint \d+: Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian./
      {
        ore: { ore: ::Regexp.last_match(1).to_i },
        clay: { ore: ::Regexp.last_match(2).to_i },
        obsidian: {
          ore: ::Regexp.last_match(3).to_i,
          clay: ::Regexp.last_match(4).to_i
        },
        geode: {
          ore: ::Regexp.last_match(5).to_i,
          clay: ::Regexp.last_match(6).to_i
        }
      }
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
