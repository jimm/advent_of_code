#!/usr/bin/env ruby
#
# Gear Ratios

require_relative '../day'
require_relative '../point'

class Day03 < Day
  class StringAndLoc
    attr_reader :str, :loc, :length
    def initialize(str, loc)
      @str = str
      @loc = loc
      @length = @str.length
    end

    def x = @loc.x
    def y = @loc.y

    # bounding box --- one char border
    def top = @loc.x - 1
    def bottom = @loc.x + 1
    def left = @loc.y - 1
    def right = @loc.y + @length

    def near_int?(int)
      int.near_symbol?(self)
    end

    def to_s = "<#{@str} @ #{@loc}>"
    def inspect = to_s
  end

  class IntAndLoc < StringAndLoc
    def to_i = @str.to_i

    def near_symbol?(sym)
      sym.x >= top && sym.x <= bottom && sym.y >= left && sym.y <= right
    end
  end

  def do_part1(lines)
    data = collect_data(lines)  # {numbers: [...], symbols: [...]}
    data[:numbers].select { near_any_symbol?(_1, data[:symbols]) }.map(&:to_i).sum
  end

  def do_part2(lines)
    data = collect_data(lines)  # {numbers: [...], symbols: [...]}
    gears = data[:symbols].select { _1.str == '*' }
    gear_nums = gears.map do |gear|
      data[:numbers].select { _1.near_symbol?(gear) }
    end
    gear_nums.select { _1.length == 2 } # only lists of 2 nums
      .map { _1.map(&:to_i) }           # convert to ints
      .map{_1.inject(&:*)}              # multiply pairs
      .sum                              # add 'em up
  end

  private

  def near_any_symbol?(int_and_loc, symbols)
    symbols.any? { int_and_loc.near_symbol?(_1) }
  end

  # Returns a hash with two keys. :number holds IntAndLocs. and :symbolx
  # holds StringAndLocs.
  def collect_data(lines)
    data = {numbers: [], symbols: []}
    lines.each_with_index do |line, row|
      col = 0
      while col < line.length
        case line[col]
        when /\d/
          line[col..] =~ /(\d+)/
          num = $1.dup
          data[:numbers] << IntAndLoc.new(num, Point.new(row, col))
          col += num.length - 1
        when '.'
          # nop
        else
          data[:symbols] << StringAndLoc.new(line[col], Point.new(row, col))
        end
        col += 1
      end
    end
    data
  end

  # def adjacent_to_symbol?(map, [pos, num_str])
  #   warn "num_str" # DEBUG
  # end

  def digit?(ch)
    return false if ch.nil? || ch == '.'

    '0' <= ch && ch <= '9'
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2023, 3)
end
