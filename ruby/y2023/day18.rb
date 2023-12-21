#!/usr/bin/env ruby
#
# Lavaduct Lagoon

require 'set'
require_relative '../day'
require_relative '../map'
require_relative '../point'

class Map
  def draw(from_row, from_col, to_row, to_col)
    if from_row == to_row
      from_col, to_col = to_col, from_col if from_col > to_col
      (from_col..to_col).each { set(from_row, _1, '#') }
    elsif from_col == to_col
      from_row, to_row = to_row, from_row if from_row > to_row
      (from_row..to_row).each { set(_1, from_col, '#') }
    else
      raise 'non-horiz/non-vert lines not yet supported'
    end
  end
end

class Day18 < Day
  DIR_TO_DELTA = {
    'R' => [0, 1],
    'L' => [0, -1],
    'U' => [-1, 0],
    'D' => [1, 0]
  }

  def do_part1(lines)
    data = parse(lines)
    points = []
    p = Point.new(0, 0)
    points << p
    data.each do |dir, dist, _|
      dx, dy = DIR_TO_DELTA[dir]
      p = Point.new(p.x + dist * dx, p.y + dist * dy)
      points << p
    end

    # # DEBUG
    # pp points # DEBUG
    # map = points_to_map(points)
    # puts map

    rectilinear_polygon_area(points)

    # sum = points.each_cons(2).map { |p0, p1| p0.x * p1.y - p0.y * p1.x }.sum
    # sum.abs / 2
  end

  def do_part2(lines)
    data = parse(lines)
    points = []
    p = Point.new(0, 0)
    points << p
    data.each do |_, _, hex|
      dist = hex >> 4
      dir = 'RDLU'[hex & 0x03]
      # warn "#{dir} #{dist}" # DEBUG
      dx, dy = DIR_TO_DELTA[dir]
      p = Point.new(p.x + dx * dist, p.y + dy * dist)
      points << p
    end
    # pp points # DEBUG
    sum = points.each_cons(2).map { |p0, p1| p0.x * p1.y - p0.y * p1.x }.sum
    sum.abs / 2
  end

  private

  def rectilinear_polygon_area(points)
    x_coords = points.map(&:x).sort.uniq
    y_coords = points.map(&:y).sort.uniq
    # warn "points = #{points}" # DEBUG
    # warn "x_coords = #{x_coords}" # DEBUG
    # warn "y_coords = #{y_coords}" # DEBUG
    # create a list of vertical line segments, with each pair going from top
    # (lower x) to bottom (higher x).
    vertical_line_segments = points
                             .each_cons(2)
                             .select { |p0, p1| p0.y == p1.y }
                             .map { |p0, p1| p0.x > p1.x ? [p1, p0] : [p0, p1] }

    # DEBUG
    map = points_to_map(points)
    points.each_cons(2) { |p0, p1| map.draw(p0.x, p0.y, p1.x, p1.y) }
    puts map

    # now go between each of the x coords, creating horizontal rectangles by
    # storing upper left and bottom right
    area = 0
    x = x_coords.min + 0.5
    maxx = x_coords.max
    while x < maxx
      relevant_line_segments = vertical_line_segments.select { |p0, p1| (p0.x..p1.x).include?(x) }
      relevant_line_segments.sort_by! { |p0, _| p0.y }
      until relevant_line_segments.empty?
        left = relevant_line_segments.shift
        right = relevant_line_segments.shift
        area += right[0].y - left[0].y + 1
      end
      x += 1.0
    end
    area
  end

  # DEBUG
  def points_to_map(points)
    minx, maxx = points.map(&:x).minmax
    miny, maxy = points.map(&:y).minmax
    m = Map.from_size(maxx - minx + 1, maxy - miny + 1)
    points.each { |p| m.set(p.x - minx, p.y - miny, '#') }
    warn "m.class.name = #{m.class.name}" # DEBUG
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
