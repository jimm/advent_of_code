#!/usr/bin/env ruby
#
# Packet Decoder

require_relative '../day'

class Day16 < Day
  class Packet
    SUM = 0
    PRODUCT = 1
    MINIMUM = 2
    MAXIMUM = 3
    LITERAL = 4
    GREATER_THAN = 5
    LESS_THAN = 6
    EQUAL = 7

    attr_reader :version, :type_id, :contents

    def initialize(version, type_id, contents)
      @version = version
      @type_id = type_id
      @contents = contents
    end

    def value
      case @type_id
      when SUM
        @contents.map(&:value).reduce(&:+)
      when PRODUCT
        @contents.map(&:value).reduce(&:*)
      when MINIMUM
        @contents.map(&:value).min
      when MAXIMUM
        @contents.map(&:value).max
      when LITERAL
        @contents
      when GREATER_THAN
        @contents[0].value > @contents[1].value ? 1 : 0
      when LESS_THAN
        @contents[0].value < @contents[1].value ? 1 : 0
      when EQUAL
        @contents[0].value == @contents[1].value ? 1 : 0
      end
    end

    def packets
      @contents
    end

    def version_sum
      return @version if @type_id == LITERAL

      @version + packets.map(&:version_sum).sum
    end
  end

  class Parser
    BITS = {
      '0' => [0, 0, 0, 0],
      '1' => [0, 0, 0, 1],
      '2' => [0, 0, 1, 0],
      '3' => [0, 0, 1, 1],
      '4' => [0, 1, 0, 0],
      '5' => [0, 1, 0, 1],
      '6' => [0, 1, 1, 0],
      '7' => [0, 1, 1, 1],
      '8' => [1, 0, 0, 0],
      '9' => [1, 0, 0, 1],
      'A' => [1, 0, 1, 0],
      'B' => [1, 0, 1, 1],
      'C' => [1, 1, 0, 0],
      'D' => [1, 1, 0, 1],
      'E' => [1, 1, 1, 0],
      'F' => [1, 1, 1, 1]
    }

    attr_reader :bits

    def self.from_hex_chars(line)
      new(line.split('').flat_map { |ch| BITS[ch] })
    end

    def initialize(bits)
      @bits = bits
    end

    # Returns a Packet
    def parse
      return nil if @bits.length < 6

      version = to_num(next_bits(3))
      type_id = to_num(next_bits(3))
      case type_id
      when Packet::LITERAL
        value_bits = []
        while 1
          group = next_bits(5)
          value_bits += group[1, 4]
          break if group[0] == 0
        end
        contents = to_num(value_bits)
        Packet.new(version, type_id, contents)
      else
        packets = []
        length_type_id = to_num(next_bits(1))
        case length_type_id
        when 0
          total_len = to_num(next_bits(15))
          bits_after = @bits[total_len..-1]
          @bits = @bits[0, total_len]
          while @bits.length >= 6
            packet = parse_next_packet
            packets << packet if packet
          end
          @bits = bits_after
        else
          num_subpackets = to_num(next_bits(11))
          num_subpackets.times do |_|
            packet = parse_next_packet
            packets << packet if packet
          end
        end
        Packet.new(version, type_id, packets)
      end
    end

    def parse_next_packet
      p = Parser.new(@bits)
      packet = p.parse
      @bits = p.bits
      packet
    end

    def to_num(bits)
      bits.join.to_i(2)
    end

    def next_bits(n)
      bs = @bits[0, n]
      @bits = @bits[n..-1]
      bs
    end
  end

  def part1
    parser = Parser.from_hex_chars(data_lines(1)[0])
    packet = parser.parse
    puts packet.version_sum
  end

  def part1_tests
    run_chunk_tests(1) do |expected, lines|
      parser = Parser.from_hex_chars(lines[0])
      packet = parser.parse
      sum = packet.version_sum
      [sum == expected.to_i, sum]
    end
  end

  def part2
    parser = Parser.from_hex_chars(data_lines(1)[0])
    packet = parser.parse
    puts packet.value
  end

  def part2_tests
    run_chunk_tests(2) do |expected, lines|
      parser = Parser.from_hex_chars(lines[0])
      packet = parser.parse
      value = packet.value
      [value == expected.to_i, value]
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
