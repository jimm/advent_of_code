#!/usr/bin/env ruby
#
# Cathode-Ray Tube

require_relative '../day'
require_relative '../map'

class Day10 < Day
  MONITOR_CYCLES = [20, 60, 100, 140, 180, 220]

  def part1
    puts do_part1(data_lines(1))
  end

  def part1_tests
    run_chunk_tests(1) do |expected, lines|
      expected = expected.to_i
      answer = do_part1(lines)
      [answer == expected, answer, expected]
    end
  end

  def part2
    puts do_part2(data_lines(1))
  end

  def part2_tests
    no_tests
  end

  private

  def do_part1(lines)
    x = 1
    cycle = 0
    sum = 0
    lines.each do |line|
      instruction, val = line.split
      val = val.to_i if val

      cycle += 1
      sum += x * cycle if MONITOR_CYCLES.include?(cycle)

      next unless instruction == 'addx'

      cycle += 1
      sum += x * cycle if MONITOR_CYCLES.include?(cycle)
      x += val
    end
    sum
  end

  def do_part2(lines)
    x = 1
    cycle = 0
    map = Map.new(Array.new(6, ' ' * 40))
    rows_and_cols = (0...(40 * 6)).map do |i|
      [i / 40, i % 40]
    end

    lines.each do |line|
      instruction, val = line.split
      val = val.to_i if val

      cycle += 1
      row, col = rows_and_cols[(cycle - 1) % rows_and_cols.length]
      map.set(row, col, '#') if sprite_on(x, col)

      next unless instruction == 'addx'

      cycle += 1
      row, col = rows_and_cols[(cycle - 1) % rows_and_cols.length]
      map.set(row, col, '#') if sprite_on(x, col)

      x += val
    end
    map.to_s_corrected
  end

  def sprite_on(x, col)
    col >= x - 1 && col <= x + 1
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc
end
