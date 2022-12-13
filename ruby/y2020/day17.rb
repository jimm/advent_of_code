#!/usr/bin/env ruby
#
# Conway Cubes

require_relative '../day'
require_relative './conway_cube'

class Day17 < Day
  def part1
    do_part(3)
  end

  def part2
    do_part(4)
  end

  def do_part(num_dimensions)
    lines = data_lines(1)
    cube = ConwayCube.new(num_dimensions, lines)
    6.times { |_| cube.next_generation }
    puts(cube.alive_count)
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2020, 17)
end
