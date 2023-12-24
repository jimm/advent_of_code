#!/usr/bin/env ruby
#
# Sand Slabs

require_relative '../day'
require_relative '../point'

class Day22 < Day
  class Brick
    def initialize(p0, p1)
      @p0 = Point.new(*p0)
      @p1 = Point.new(*p1)
    end
  end

  def do_part1(lines)
    data = parse(lines)
  end

  def do_part2(lines)
    # TODO
  end

  private

  def parse(lines)
    lines.map do |line|
      coords = line.split('~').map { _1.split(',').map(&:to_i) }
      Brick.new(*coords)
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
