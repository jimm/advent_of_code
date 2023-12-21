#!/usr/bin/env ruby
#
# Step Counter

require_relative '../day'
require_relative '../map'

class Day21 < Day
  def do_part1(lines)
    map = Map.new(lines)
    start = map.find('S')
    map.set(start[0], start[1], '.')
    num_steps = @testing ? 6 : 64
    nth_step_count(map, start, num_steps)
  end

  def do_part2(lines)
    map = Map.new(lines, :both)
    start = map.find('S')
    map.set(start[0], start[1], '.')
    num_steps = @testing ? 100 : 26_501_365
    nth_step_count(map, start, num_steps)
  end

  private

  def nth_step_count(map, loc, num_steps)
    queue = Set.new
    queue << loc
    num_steps.times do
      new_queue = Set.new
      max_height = map.height - 1
      max_width = map.width - 1
      queue.each do |loc|
        [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |dr, dc|
          new_loc = [loc[0] + dr, loc[1] + dc]
          new_queue << new_loc if map.at(*new_loc) == '.'
        end
      end
      queue = new_queue
    end
    queue.each { |r, c| map.set(r, c, 'O') }
    queue.size
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
