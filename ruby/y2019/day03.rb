#!/usr/bin/env ruby
#
# Crossed Wires

require_relative '../day'
require_relative '../point'

class Day03 < Day
  def do_part1(lines)
    wire1 = coords_from_directions(lines[0])
    wire2 = coords_from_directions(lines[1])
    intersections = (wire1 & wire2) - [Point::ORIGIN]
    intersections.map(&:manhattan_distance).min
  end

  def do_part2(lines)
    wire1 = coords_from_directions(lines[0])
    wire2 = coords_from_directions(lines[1])
    intersections = (wire1 & wire2) - [Point::ORIGIN]
    # This isn't the most efficient approach, but it'll do
    distance_sums = intersections.map do |p|
      distance_to(wire1, p) + distance_to(wire2, p)
    end
    distance_sums.min
  end

  private

  def distance_to(wire, p)
    wire.index(p)
  end

  def coords_from_directions(line)
    here = Point::ORIGIN
    coords = [here]
    dirs = line.split(',')
    dirs.each do |dir|
      case dir[0]
      when 'U'
        dx = 0
        dy = 1
      when 'D'
        dx = 0
        dy = -1
      when 'L'
        dx = -1
        dy = 0
      when 'R'
        dx = 1
        dy = 0
      end
      count = dir[1..].to_i
      count.times do |_|
        here = Point.new(here.x + dx, here.y + dy)
        coords << here
      end
    end
    coords
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2019, 3)
end
