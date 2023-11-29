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
    # TODO
  end

  private

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
