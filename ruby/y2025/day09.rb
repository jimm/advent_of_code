#!/usr/bin/env ruby
#
# Movie Theater

require_relative '../day'
require_relative '../point'

class Day09 < Day
  def do_part1(lines)
    parse_2d(lines)
      .combination(2)
      .map { |p0, p1| p2_area(p0, p1) }
      .max
  end

  # A rectangle is an array of [x_min, x_max, y_min, y_max].
  # An edge is an array of [p0_x, p0_y, p1_x, p1_y].
  def do_part2(lines)
    red_tiles = parse_2d(lines)
    # edges = red_tiles.each_cons(2).to_a + [[red_tiles[-1], red_tiles[0]]]
    # horiz_edges, vert_edges = edges.partition { |edge| edge[0].y == edge[1].y }

    edges = (red_tiles.each_cons(2).to_a + [[red_tiles[-1], red_tiles[0]]])
            .map do |p0, p1|
      x_min, x_max = [p0, p1].map(&:x).minmax
      y_min, y_max = [p0, p1].map(&:y).minmax
      [x_min, y_min, x_max, y_max]
    end

    max_area = 0
    red_tiles
      .combination(2)                     # two-corner pairs of rect corners
      .map { |p0, p1| rect_from(p0, p1) } # produce array of [x_min, y_min, x_max, y_max]
      # .select { |rect| rect == [2, 1, 11, 5] }
      .each do |rect| # calc area, if > max and in-bounds save area else reject
        area = rect_area(rect)
        max_area = area if (area > max_area) && !crosses_any_edge?(rect, edges)
      end
    max_area
  end

  private

  # Returns an array of [x_min, y_min, x_max, y_max].
  def rect_from(p0, p1)
    x_min, x_max = [p0, p1].map(&:x).minmax
    y_min, y_max = [p0, p1].map(&:y).minmax
    [x_min, y_min, x_max, y_max]
  end

  def crosses_any_edge?(rect, edges)
    x_min, y_min, x_max, y_max = *rect

    edges.any? do |edge|
      e_x_min, e_x_max = [edge[0], edge[2]].minmax
      e_y_min, e_y_max = [edge[1], edge[3]].minmax
      x_min < e_x_max && x_max > e_x_min && y_min < e_y_max && y_max > e_y_min
    end
  end

  # Given two corners of a rectangle, return the area
  def p2_area(p0, p1)
    ((p1.x - p0.x).abs + 1) * ((p1.y - p0.y).abs + 1)
  end

  # Given two corners of a rectangle, return the area
  def rect_area(rect)
    x_min, y_min, x_max, y_max = *rect
    (x_max - x_min + 1) * (y_max - y_min + 1)
  end

  def parse_2d(lines)
    lines.map { |line| Point.new(*line.split(',').map(&:to_i)) }
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
