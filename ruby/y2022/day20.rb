#!/usr/bin/env ruby
#
# Grove Positioning System

require_relative '../day'

class Day20 < Day
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
    seq = parse(lines)
    mixed = mix(seq)
    offset = mixed.index(0)
    mixed[(1000 + offset) % mixed.length] +
      mixed[(2000 + offset) % mixed.length] +
      mixed[(3000 + offset) % mixed.length]
  end

  def parse(lines)
    lines.map(&:to_i)
  end

  def mix(seq)
    len = seq.length
    mixed = seq.dup
    seq.each do |n|
      next if n == 0

      i = mixed.index(n)
      mixed.delete(n)
      new_i = i + n
      if new_i <= 0
        new_i -= 1
        new_i += len while new_i < 0
      elsif new_i >= len
        new_i = (new_i % len) + 1
      end
      mixed.insert(new_i, n)
    end
    mixed
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2022, 20)
end
