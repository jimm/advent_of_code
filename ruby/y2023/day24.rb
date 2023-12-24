#!/usr/bin/env ruby
#
# Never Tell Me The Odds

require_relative '../day'
require_relative '../point'

class Day24 < Day
  class Hailstone
    attr_reader :pos, :vel

    def initialize(pos, vel)
      @pos = pos
      @vel = vel
    end

    def to_s = "Hailstone(#{@pos.x}, #{@pos.y}, #{@pos.z} @ #{@vel.x}, #{@vel.y}, #{@vel.z})"
  end

  def do_part1(lines)
    hailstones = parse(lines)
    window = @testing ? (7..27) : (200000000000000..400000000000000)
    count_intersecting_paths(hailstones, window)
  end

  def do_part2(lines)
    # TODO
  end

  private

  def count_intersecting_paths(hailstones, range)
    count = 0
    hailstones.combination(2).each do |h0, h1|
     count +=1 if paths_intersect(h0, h1, range)
    end
    count
  end

  def paths_intersect(h0, h1, range)
    x1, y1 = h0.pos.to_a.map(&:to_f)
    x2, y2 = another_point(h0).to_a.map(&:to_f)
    x3, y3 = h1.pos.to_a.map(&:to_f)
    x4, y4 = another_point(h1).to_a.map(&:to_f)

    denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    return false if denom == 0  # they never intersection

    numer_x = (x1 * y2 - y1 * x2) * (x3 - x4) -
              (x1 - x2) * (x3 * y4 - y3 * x4)
    numer_y = (x1 * y2 - y1 * x2) * (y3 - y4) -
              (y1 - y2) * (x3 * y4 - y3 * x4)

    # px, py is the intersection point
    px = numer_x / denom
    py = numer_y / denom

    in_range = range.include?(px) && range.include?(py)
    return false unless in_range

    # Now we have to look at the time element. Only return true if the
    # collision will be in the future.
    return false if h0.vel.x > 0 && px < h0.pos.x
    return false if h0.vel.x < 0 && px > h0.pos.x
    return false if h0.vel.y > 0 && py < h0.pos.y
    return false if h0.vel.y < 0 && py > h0.pos.y
    return false if h1.vel.x > 0 && px < h1.pos.x
    return false if h1.vel.x < 0 && px > h1.pos.x
    return false if h1.vel.y > 0 && py < h1.pos.y
    return false if h1.vel.y < 0 && py > h1.pos.y
    true
  end

  def another_point(hailstone)
    Point.new(
      hailstone.pos.x + hailstone.vel.x,
      hailstone.pos.y + hailstone.vel.y
    )
  end

  def parse(lines)
    lines.map do |line|
      pos, vel = line.split('@').map do |vals|
        Point.new(*vals.split(', ').map(&:to_i))
      end
      Hailstone.new(pos, vel)
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
