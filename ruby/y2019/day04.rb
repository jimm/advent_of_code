#!/usr/bin/env ruby
#
# Secure Container

require_relative '../day'

class Day04 < Day
  PUZZLE_INPUT = [271_973, 785_961]

  def do_part1(lines)
    num_valid_passwords = 0
    Range.new(*PUZZLE_INPUT).each do |i|
      num_valid_passwords += 1 if valid_password(i, true)
    end
    num_valid_passwords
  end

  def do_part2(lines)
    num_valid_passwords = 0
    Range.new(*PUZZLE_INPUT).each do |i|
      num_valid_passwords += 1 if valid_password(i, false)
    end
    num_valid_passwords
  end

  private

  def valid_password(i, more_than_two_ok)
    digits = i.to_s.chars.map(&:to_i)
    prev_digit = -1
    digits.each do |digit|
      return false if digit < prev_digit

      prev_digit = digit
    end
    runs = digits.group_by(&:to_i)
    return runs.values.any? { |digits| digits.length >= 2 } if more_than_two_ok

    runs.values.any? { |digits| digits.length == 2 }
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2019, 4)
end
