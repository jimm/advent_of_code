#!/usr/bin/env ruby
#
# Rucksack Reorganization

require_relative '../day'

class Day03 < Day
  def part1
    lines = data_lines(1)
    puts do_part1(lines)
  end

  def part1_tests
    run_chunk_tests(1) do |expected, lines|
      expected = expected.split(', ')[0].to_i
      answer = do_part1(lines)
      [answer == expected, answer]
    end
  end

  def part2
    lines = data_lines(1)
    puts do_part2(lines)
  end

  def part2_tests
    run_chunk_tests(1) do |expected, lines|
      expected = expected.split(', ')[1].to_i
      answer = do_part2(lines)
      [answer == expected, answer]
    end
  end

  private

  # Returns the sum of the scores of the letters common to each rucksack.
  def do_part1(lines)
    lines_to_rucksacks(lines)
      .map { letter_score(find_common_letter(_1)) }
      .sum
  end

  # Returns the sum of the scores of the letters common to each group of
  # three rucksacks.
  def do_part2(lines)
    lines_to_rucksacks(lines)
      .each_slice(3)
      .map { letter_score(find_common_group_letter(_1)) }
      .sum
  end

  # Returns rucksacks, which are arrays of two arrays of chars.
  def lines_to_rucksacks(lines)
    lines.map do |line|
      items = line.split('')
      half = items.length / 2
      [items[0, half], items[half..]]
    end
  end

  # Returns the letter that is in both sides of the `rucksack`.
  def find_common_letter(rucksack)
    (rucksack[0] & rucksack[1])[0]
  end

  # Returns the letter that is common among all rucksacks in `group`.
  def find_common_group_letter(group)
    group.map(&:flatten).reduce(:&).first
  end

  # Returns the score of `letter`.
  def letter_score(letter)
    ('a'..'z').include?(letter) ? (letter.ord - 'a'.ord + 1) : (letter.ord - 'A'.ord + 27)
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2022, 3)
end
