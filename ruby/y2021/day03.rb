#!/usr/bin/env ruby
#
# Binary Diagnostic

require_relative '../day'

class Day03 < Day
  def part1
    gamma_rate = epsilon_rate = 0
    mcb = most_common_bits(data_lines(1))
    mcb.each_with_index do |bit, power|
      if bit == 1
        gamma_rate += 2**power
      else
        epsilon_rate += 2**power
      end
    end
    puts gamma_rate * epsilon_rate
  end

  def part2
    num_strings = data_lines(1)

    nums = num_strings.dup
    bit = 0
    while nums.length > 1
      mcb = most_common_bits(nums).reverse # we want high bit first
      nums.select! { |str| str[bit] == (mcb[bit] == 0 ? '0' : '1') }
      bit += 1
    end
    o_generator_rating = nums[0].to_i(2)

    nums = num_strings.dup
    bit = 0
    while nums.length > 1
      mcb = most_common_bits(nums).reverse
      nums.select! { |str| str[bit] == (mcb[bit] == 0 ? '1' : '0') }
      bit += 1
    end
    co2_scrubber_rating = nums[0].to_i(2)

    puts o_generator_rating * co2_scrubber_rating
  end

  # Returns an array containing the most common bit, 0 or 1, for each power
  # of two from the input numbers. If there are an equal number of 0s and 1s
  # then a -1 is returned in its spot. Array element 0 is the low bit.
  def most_common_bits(num_strings)
    num_bits = num_strings[0].length
    ones = [0] * num_bits # index 0 = low bit
    zeroes = [0] * num_bits
    num_strings.each do |str|
      power = 0
      str.reverse.each_char do |ch|
        if ch == '0'
          zeroes[power] += 1
        else
          ones[power] += 1
        end
        power += 1
      end
    end

    most_common = []
    num_bits.times do |power|
      most_common << if ones[power] > zeroes[power]
                       1
                     elsif ones[power] < zeroes[power]
                       0
                     else
                       -1
                     end
    end
    most_common
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc
end
