#!/usr/bin/env ruby
#
# Docking Data

require_relative '../day'

class MaskingBootloader
  ALL_ONES = 0xfffffffff # 36 bits

  def initialize
    @mask_ones = 0
    @mask_zeroes = ALL_ONES
    @memory = {}
  end

  def parse_and_load(lines)
    lines.each do |line|
      if line[0, 7] == 'mask = '
        parse_mask(line[7..-1])
      else
        line =~ /mem\[(\d+)\] = (\d+)/
        set_memory(::Regexp.last_match(1).to_i, ::Regexp.last_match(2).to_i)
      end
    end
  end

  def parse_mask(mask_str)
    raise 'subclasses must implement'
  end

  def set_memory(address, value)
    raise 'subclasses must implement'
  end

  def memory_sum
    @memory.values.sum
  end

  # for debugging
  def b(value)
    '%036b' % value
  end
end

class MaskingBootloaderV1 < MaskingBootloader
  def parse_mask(mask_str)
    @mask_ones = 0
    @mask_zeroes = ALL_ONES

    bit_pos = 2**35
    mask_str.each_char do |ch|
      case ch
      when '0'
        @mask_zeroes &= (ALL_ONES - bit_pos)
      when '1'
        @mask_ones |= bit_pos
      end
      bit_pos >>= 1
    end
  end

  def set_memory(address, value)
    value |= @mask_ones
    value &= @mask_zeroes
    @memory[address] = apply_mask(value)
  end
end

class MaskingBootloaderV2 < MaskingBootloader
  def parse_mask(mask_str)
    @mask_ones = 0
    @mask_zeroes = ALL_ONES
    @mask_floating_bit_pos = []

    bit_pos = 2**35
    mask_str.each_char do |ch|
      case ch
      when '0'
        @mask_zeroes &= (ALL_ONES - bit_pos)
      when '1'
        @mask_ones |= bit_pos
      else
        @mask_floating_bit_pos << bit_pos
      end
      bit_pos >>= 1
    end
  end

  def set_memory(address, value)
    address |= @mask_ones
    set_floating_memory(address, value, @mask_floating_bit_pos)
  end

  # Recursively sets each `floating_bit_pos` to 0 and to 1 and writes
  # `value` to the resulting modifications of `address`.
  def set_floating_memory(address, value, floating_bit_pos)
    if floating_bit_pos.empty?
      @memory[address] = value
      return
    end

    bit_pos = floating_bit_pos[0]
    rest_bit_pos = floating_bit_pos[1..-1]
    set_floating_memory(address & (ALL_ONES - bit_pos), value, rest_bit_pos)
    set_floating_memory(address | bit_pos, value, rest_bit_pos)
  end
end

class Day14 < Day
  def part1
    lines = data_lines(1)
    mb = MaskingBootloaderV1.new
    mb.parse_and_load(lines)
    puts(mb.memory_sum)
  end

  def part2
    lines = data_lines(2)
    mb = MaskingBootloaderV2.new
    mb.parse_and_load(lines)
    puts(mb.memory_sum)
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
