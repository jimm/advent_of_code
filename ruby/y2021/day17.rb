#!/usr/bin/env ruby
#
# Trick Shot

require_relative '../day'
require_relative '../utils'

class Day17 < Day
  def part1
    lines = data_lines(1)
    target = read_target(lines)

    max_heights = []
    101.times do |x_velocity|
      101.times do |y_velocity|
        max_heights << max_height_for_trajectory(target, x_velocity, y_velocity)
      end
    end
    max_heights.compact!
    puts max_heights.max

    # pp target                   # DEBUG
    # x_velocity_range = calc_x_range(target[:x])
    # x_velocity_range = (0..100) # DEBUG
    # puts x_velocity_range       # DEBUG
    # max_heights = x_velocity_range.map { |x| max_height(target, x) }
    # puts max_heights.inspect    # DEBUG
    # puts max_heights.max
  end

  def part1_tests
    run_chunk_tests(1) do |expected, lines|
      target = read_target(lines)
      x_velocity_range = calc_x_range(target[:x])
      max_heights = x_velocity_range.map { |x| max_height(target, x) }
      val = max_heights.max
      [val == expected.split(',')[1].to_i, val]
    end
  end

  # 396 is too low
  def part2
    lines = data_lines(1)
    target = read_target(lines)

    vels = []
    (0..(target[:x].max + 1)).each do |x_velocity|
      y_bounds = target[:y].min - 1
      y_bounds = y_bounds.abs if y_bounds < 0
      (-y_bounds..y_bounds).each do |y_velocity|
        vels << [x_velocity, y_velocity] if intersects?(target, x_velocity, y_velocity)
      end
    end
    puts vels.length
  end

  def part2_tests
    run_chunk_tests(1) do |expected, lines|
      target = read_target(lines)

      vels = []
      (0..(target[:x].max + 1)).each do |x_velocity|
        y_bounds = target[:y].min - 1
        y_bounds = y_bounds.abs if y_bounds < 0
        (-y_bounds..y_bounds).each do |y_velocity|
          vels << [x_velocity, y_velocity] if intersects?(target, x_velocity, y_velocity)
        end
      end

      val = vels.length
      [val == expected.split(',')[1].to_i, val]
    end
  end

  def read_target(lines)
    line = lines[0]
    line =~ /target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)/
    {
      x: (Regexp.last_match(1).to_i..Regexp.last_match(2).to_i),
      y: (Regexp.last_match(3).to_i..Regexp.last_match(4).to_i)
    }
  end

  def on_target?(x, y, target)
    target[:x].include?(x) && target[:y].include?(y)
  end

  def past_target?(x, y, target)
    x > target[:x].max || y < target[:y].min
  end

  # 6, 0 and 7, -1 not being reported as on target
  def intersects?(target, x_velocity, y_velocity)
    x = 0
    y = 0
    while 1
      return true if on_target?(x, y, target)
      return false if past_target?(x, y, target)

      x += x_velocity
      y += y_velocity
      x_velocity -= 1 if x_velocity > 0
      y_velocity -= 1
    end
  end

  def max_height(target, x_velocity)
    max_h = 0
    y_velocity = 1
    # try different y velocities
    while 1
      h = max_height_for_trajectory(target, x_velocity, y_velocity)
      if h                      # nil means a complete miss
        max_h = h if h > max_h
      elsif max_h > 0
        return max_h
      end
      y_velocity += 1
    end
    max_h
  end

  def max_height_for_trajectory(target, x_velocity, y_velocity)
    trajectory = []

    x = 0
    y = 0
    while 1
      x += x_velocity
      y += y_velocity
      trajectory << [x, y]
      return trajectory.max_by { |x, y| y }[1] if on_target?(x, y, target)
      return nil if past_target?(x, y, target)

      x_velocity -= 1 if x_velocity > 0
      y_velocity -= 1
    end
  end

  def calc_x_range(target)
    twice_range = (target.min * 2..target.max * 2)
    x_min = 0
    x_max = nil
    x = 0
    x += 1 while (x * x + x) < twice_range.min
    x_min = x
    x += 1 while (x * x + x) < twice_range.max
    x_max = x - 1
    (x_min..x_max)
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
