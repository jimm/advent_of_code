#!/usr/bin/env ruby
#
# Lavaduct Lagoon

require 'set'
require_relative '../day'
require_relative '../map'
require_relative '../point'

class Day18 < Day
  DIR_TO_DELTA = {
    'R' => [0, 1],
    'L' => [0, -1],
    'U' => [-1, 0],
    'D' => [1, 0]
  }

  def do_part1(lines)
    data = parse(lines)
    points = Set.new
    p = Point.new(0, 0)
    points.add(p)
    data.each do |dir, dist, _color|
      dx, dy = DIR_TO_DELTA[dir]
      dist.times do
        p = Point.new(p.x + dx, p.y + dy)
        points << p
      end
    end
    map = points_to_map(points)
    ri, ci = map.find('#')
    map.flood_fill(ri + 1, ci + 1, '.', '#')
    count = 0
    map.each { |_, _, ch| count += 1 if ch == '#' }
    count
  end

  def do_part2(lines)
    data = parse(lines)
    points = []
    p = Point.new(0, 0)
    points << p
    data.each do |a, b, c|
      dist = c >> 4
      dir = 'RDLU'[c & 0x03]
      dx, dy = DIR_TO_DELTA[dir]
      p = Point.new(p.x + (dx * (dist - 1)) - 1, p.y + (dy * (dist - 1)))
      points << p
    end
    points << points[0]
    sum = 0
    points.each_cons(2) { |p0, p1| sum += p0.x * p1.y - p0.y * p1.x }
    (sum / 2.0).abs
  end

  private

  def points_to_map(points)
    minx, maxx = points.map(&:x).minmax
    miny, maxy = points.map(&:y).minmax
    m = Map.from_size(maxx - minx + 1, maxy - miny + 1)
    points.each { |p| m.set(p.x - minx, p.y - miny, '#') }
    m
  end

  def parse(lines)
    lines.map do |line|
      line =~ /(.) (\d+) \(#(.{6})/
      dir = ::Regexp.last_match(1)
      dist = ::Regexp.last_match(2)
      color = ::Regexp.last_match(3)
      dist = dist.to_i
      color = color.to_i(16)
      [dir, dist, color]
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
