#!/usr/bin/env ruby
#
# Camp Cleanup

require_relative '../day'

class Day04 < Day
  def part1
    puts do_part(:fully_contains?, data_lines(1))
  end

  def part1_tests
    do_test(:fully_contains?)
  end

  def part2
    puts do_part(:any_overlap?, data_lines(1))
  end

  def part2_tests
    do_test(:any_overlap?)
  end

  private

  def do_part(predicate, lines)
    num_fully_contained = 0
    lines.each do |line|
      num_fully_contained += 1 if send(predicate, *parse_line(line))
    end
    num_fully_contained
  end

  def do_test(predicate)
    run_chunk_tests(1) do |expected, lines|
      expected = expected.split(',')[@part_number - 1].to_i
      answer = do_part(predicate, lines)
      [answer == expected, answer]
    end
  end

  def fully_contains?(a, b)
    covered?(a, b) || covered?(b, a)
  end

  def covered?(a, b)
    a[0] <= b[0] && a[1] >= b[1]
  end

  def any_overlap?(a, b)
    overlaps?(a, b) || overlaps?(b, a)
  end

  def overlaps?(a, b)
    (a[1] >= b[0] && a[0] <= b[0]) ||
      (a[0] <= b[1] && a[1] >= b[1])
  end

  def parse_line(line)
    line.split(',').map { _1.split('-').map(&:to_i) }
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc
end
