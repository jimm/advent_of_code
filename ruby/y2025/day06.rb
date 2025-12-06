#!/usr/bin/env ruby
#
# Trash Compactor

require_relative '../day'
require_relative '../map'

class Day06 < Day
  class Problem
    attr_accessor :numbers, :operation

    def initialize(numbers = [], operation = :+)
      @numbers = numbers
      @operation = operation
    end

    def apply
      @operation == :+ ? 0 : 1
      @numbers.reduce { |acc, val| acc.send(@operation, val) }
    end
  end

  def do_part1(lines)
    parse_problems(lines).map(&:apply).sum
  end

  def do_part2(lines)
    parse_problems_the_cephalopod_way(lines).map(&:apply).sum
  end

  private

  def parse_problems(lines)
    number_rows = lines[..-2].map { |line| line.split.map(&:to_i) }
    ops = lines[-1].split.map(&:to_sym)
    problems = []
    ops.length.times do |i|
      problems << Problem.new(number_rows.map { |row| row[i] }, ops[i])
    end
    problems
  end

  def parse_problems_the_cephalopod_way(lines)
    m = Map.new(lines)
    problems = []
    problem = Problem.new
    (m.width - 1).downto(0).each do |col|
      n = 0
      op = nil
      m.height.times do |row|
        ch = m.at(row, col)
        if digit?(ch)
          n = n * 10 + ch.to_i
        elsif ['+', '*'].include?(ch)
          op = ch.to_sym
        end
      end
      next unless n != 0

      problem.numbers << n
      next if op.nil?

      problem.operation = op
      problems << problem
      problem = Problem.new
    end
    # ops are at left of column so we don't want to add the lastempty
    # problem to the problems array.
    problems
  end

  def digit?(ch)
    ch >= '0' && ch <= '9'
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
