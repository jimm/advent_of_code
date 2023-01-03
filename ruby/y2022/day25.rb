#!/usr/bin/env ruby
#
# Full of Hot Air

require_relative '../day'

class Day25 < Day
  SNAFU_TO_DECIMAL = {
    '2' => 2,
    '1' => 1,
    '0' => 0,
    '-' => -1,
    '=' => -2
  }
  DECIMAL_TO_SNAFU = ['0', '1', '2', '=', '-']

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
      expected = expected.split(',')[@part_number - 1]
      answer = do_part(lines)
      [answer == expected, answer, expected]
    end
  end

  def do_part(lines)
    decimal_to_snafu(
      lines.map { |line| snafu_to_decimal(line) }.sum
    )
  end

  def snafu_to_decimal(snafu)
    decimal = 0
    snafu.each_char do |ch|
      decimal *= 5
      decimal += SNAFU_TO_DECIMAL[ch]
    end
    decimal
  end

  def decimal_to_snafu(decimal)
    snafu = ''
    while decimal > 0
      decimal, rem = decimal.divmod(5)
      decimal += 1 if rem >= 3
      snafu << DECIMAL_TO_SNAFU[rem]
    end
    snafu.reverse
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2022, 25)
end
