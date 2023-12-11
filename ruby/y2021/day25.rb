#!/usr/bin/env ruby
#
# Sea Cucumber

require_relative '../day'
require_relative '../map'

class Day25 < Day
  class SeaCucumberMap < Map
    attr_reader :num_moved

    def step
      @num_moved = 0
      copy = dup

      # move horizontal cukes
      @height.times.each do |row|
        (@width - 1).downto(0).each do |col|
          next unless at(row, col) == '>' && at(row, col + 1) == '.'

          copy.set(row, col, '.')
          copy.set(row, col + 1, '>')
          @num_moved += 1
        end
      end
      @rows = copy.rows
      copy = dup

      # move vertical cukes
      (@height - 1).downto(0).each do |row|
        @width.times.each do |col|
          next unless at(row, col) == 'v' && at(row + 1, col) == '.'

          copy.set(row, col, '.')
          copy.set(row + 1, col, 'v')
          @num_moved += 1
        end
      end

      @rows = copy.rows
    end

    def column(n)
      @rows.map { |row| row[n] }
    end

    def dup
      duplicate = super
      duplicate.instance_variable_set(:@rows, duplicate.rows.dup)
      (0..height - 1).each do |row|
        duplicate.rows[row] = duplicate.rows[row].dup
      end
      duplicate
    end
  end

  def part1
    lines = data_lines(1)
    map = SeaCucumberMap.new(lines, :both)
    step = 0
    while true
      step += 1
      map.step
      break if map.num_moved == 0
    end
    puts step
  end

  def part1_tests
    run_chunk_tests(1) do |expected, lines|
      expected = eval(expected)
      map = SeaCucumberMap.new(lines, :both)
      step = 0
      while true
        step += 1
        map.step
        break if map.num_moved == 0
      end
      pp expected
      [step == expected[:stop_step], step]
    end
  end

  def part2
    lines = data_lines(1)
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
