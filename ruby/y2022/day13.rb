#!/usr/bin/env ruby
#
# Distress Signal

require_relative '../day'

class Day13 < Day
  def part1
    puts do_part1(data_lines(1))
  end

  def part1_tests
    do_tests
  end

  def part2
    puts do_part2(data_lines(1))
  end

  def part2_tests
    do_tests(['[[2]]', '[[6]]'])
  end

  private

  def do_tests(additional_lines = [])
    run_chunk_tests(1) do |expected, lines|
      expected = expected.split(',')[@part_number - 1].to_i
      answer = send(:"do_part#{@part_number}", lines + additional_lines)
      [answer == expected, answer, expected]
    end
  end

  def do_part1(lines)
    in_order_indexes = []
    index = 0
    lines.each_slice(2) do |line1, line2|
      index += 1
      signal1 = eval(line1)
      signal2 = eval(line2)
      x = compare(signal1, signal2)
      in_order_indexes << index if x < 0
    end
    in_order_indexes.sum
  end

  def do_part2(lines)
    lines += ['[[2]]', '[[6]]']
    in_order_indexes = []
    index = 0
    signals = lines.map { eval(_1) }
    sorted = signals.sort { |sig1, sig2| compare(sig1, sig2) }
    divider_packet_1_index = sorted.index([[2]]) + 1
    divider_packet_2_index = sorted.index([[6]]) + 1
    divider_packet_1_index * divider_packet_2_index
  end

  def compare(signal1, signal2)
    signal1 += [nil] * (signal2.length - signal1.length) if signal1.length < signal2.length
    signal1.zip(signal2).each do |v1, v2|
      return -1 if v1.nil? # signal1 is shorter
      return 1 if v2.nil? # signal2 is shorter
      next if v1 == v2

      if v1.is_a?(Integer) && v2.is_a?(Integer)
        return -1 if v1 < v2
        return 1 if v1 > v2
      end

      if v1.is_a?(Array) && v2.is_a?(Array)
        cmp = compare(v1, v2)
        return cmp unless cmp == 0 # if same, continue to next element
      end

      if v1.is_a?(Integer)
        cmp = compare([v1], v2)
        return cmp unless cmp == 0
      end

      if v2.is_a?(Integer)
        cmp = compare(v1, [v2])
        return cmp unless cmp == 0
      end
    end
    0
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
