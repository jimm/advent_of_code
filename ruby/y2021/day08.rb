#!/usr/bin/env ruby
#
# Seven Segment Search

require_relative '../day'

class Day08 < Day
  # Digits 0-9 encoded as seven-segment displays as described in the puzzle
  CORRECT = %w[
    abcdfg cf acdeg acdfg bcdf
    abdfg abdefg acf abcdefg abcdfg
  ]

  #  aaaa
  # b    c
  # b    c
  #  dddd
  # e    f
  # e    f
  #  gggg
  #
  # Unique digits are
  # - 1 (c, f)
  # - 7 (a, c, f)
  # - 4 (b, c, d, f)
  # - 8 (a, b, c, d, e, f, g)
  #
  # c, f are the two right segments.
  #
  # Other digits and the segments they have
  # - 2 (5) (common a,d,g; uses c,e)
  # - 3 (5) (common a,d,g; uses c,f) (same as 1)
  # - 5 (5) (common a,d,g; uses b,f) (same as 4)
  # - 0 (6) (common a,b,f,g; uses c,e)
  # - 6 (6) (common a,b,f,g; uses d,e) (same as 4)
  # - 9 (6) (common a,b,f,g; uses c,d) (contains all of "4")

  class Digit
    def initialize(value, segments)
    end
  end

  def part1
    lines = data_lines(1)
    num_digits_unique_segment_counts = 0
    lines.each do |line|
      inputs, output_four_digits = line.split('|').map(&:strip).map(&:split)
      # All we need to check are the lengths of the scrambled outputs. We
      # don't need to unscramble anything.
      output_four_digits.each do |s|
        num_digits_unique_segment_counts += 1 if [2, 4, 3, 7].include?(s.length)
      end
    end
    puts num_digits_unique_segment_counts
  end

  def part2
    lines = data_lines(1)
    total = 0
    lines.each do |line|
      inputs, output_digits = line.split('|').map(&:strip).map(&:split)
      val = unscramble(inputs, output_digits)
      total += val
    end
    puts total
  end

  def unscramble(inputs, output_digits)
    # Order segments alphabetically. We don't NEED to do this, but it might
    # speed things up a bit.
    inputs.map! { |s| s.split('').sort }
    output_digits.map! { |s| s.split('').sort }

    inputs_by_len = inputs.group_by { |s| s.length }
    # index = real digit, value = array of segment chars that matches
    digit_map = []
    digit_map[1] = inputs_by_len[2][0]
    digit_map[7] = inputs_by_len[3][0]
    digit_map[4] = inputs_by_len[4][0]
    digit_map[8] = inputs_by_len[7][0]
    four_diff = digit_map[4] - digit_map[1]

    len_group = inputs_by_len[5]
    digit_map[3] = len_group.detect { |s| digit_map[1].all? { |ch| s.include?(ch) } }
    digit_map[5] = len_group.detect { |s| four_diff.all? { |ch| s.include?(ch) } }
    digit_map[2] = len_group.reject { |s| s == digit_map[3] || s == digit_map[5] }[0]

    len_group = inputs_by_len[6]
    digit_map[9] = len_group.detect { |s| digit_map[4].all? { |ch| s.include?(ch) } }
    digit_map[6] = len_group.detect { |s| four_diff.all? { |ch| s.include?(ch) } && s != digit_map[9] }
    digit_map[0] = len_group.reject { |s| s == digit_map[9] || s == digit_map[6] }[0]

    output_digits.map! do |chars|
      digit_map.index(chars)
    end
    output_digits[0] * 1000 + output_digits[1] * 100 +
      output_digits[2] * 10 + output_digits[3]
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc
end
