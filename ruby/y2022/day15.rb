#!/usr/bin/env ruby
#
# Beacon Exclusion Zone

require 'set'
require_relative '../day'
require_relative '../point'

class Day15 < Day
  def part1
    puts do_part1(data_lines(1), @testing ? 10 : 2_000_000)
  end

  def part1_tests
    do_tests(@testing ? 10 : 2_000_000)
  end

  def part2
    puts do_part2(data_lines(1), @testing ? 20 : 4_000_000)
  end

  def part2_tests
    do_tests(@testing ? 20 : 4_000_000)
  end

  private

  def do_tests(arg)
    run_chunk_tests(1) do |expected, lines|
      expected = expected.split(',')[@part_number - 1].to_i
      answer = send(:"do_part#{@part_number}", lines, arg)
      [answer == expected, answer, expected]
    end
  end

  def do_part1(lines, y_row)
    sensors_and_beacons, occupied = setup(lines)
    not_beacon_xs = Set.new
    sensors_and_beacons.each do |sensor, beacon, mdist|
      add_not_beacon_xs(sensor, beacon, mdist, y_row, occupied, not_beacon_xs)
    end
    not_beacon_xs.size
  end

  def add_not_beacon_xs(sensor, beacon, mdist, y_row, occupied, not_beacon_xs)
    y_dist = (sensor.y - y_row).abs
    x_diff = mdist - y_dist
    if x_diff > 0
      (-x_diff..x_diff).each do |dx|
        p = Point.new(sensor.x + dx, y_row)
        not_beacon_xs.add(p) unless occupied.include?(p)
      end
    end
  end

  def do_part2(lines, max_coord)
    sensors_and_beacons, occupied = setup(lines)

    min_x = [0, sensors_and_beacons.map { |sb| sb[0].x - sb[2] }.min].max
    max_x = [max_coord, sensors_and_beacons.map { |sb| sb[0].x + sb[2] }.max].min
    min_y = [0, sensors_and_beacons.map { |sb| sb[0].y - sb[2] }.min].max
    max_y = [max_coord, sensors_and_beacons.map { |sb| sb[0].y + sb[2] }.max].min

    p = Point.new
    (min_x..max_x).each do |x|
      p.x = x
      (min_y..max_y).each do |y|
        p.y = y
        return (p.x * 4_000_000 + p.y) if free_point?(sensors_and_beacons, occupied, p)
      end
    end
    nil
  end

  def free_point?(sensors_and_beacons, occupied, p)
    return false if occupied.include?(p)

    return p if sensors_and_beacons.all? do |sbm|
      sensor, beacon, mdist = *sbm
      sensor.manhattan_distance(p) > mdist
    end

    nil
  end

  # Returns sensors_and_beacons Array and occupied Set.
  def setup(lines)
    occupied = Set.new
    sensors_and_beacons = parse(lines)
    sensors_and_beacons.each do |sensor, beacon, _|
      occupied.add(sensor)
      occupied.add(beacon)
    end
    [sensors_and_beacons, occupied]
  end

  # Returns array of [sensor, beacon, Manhattan distance]. Returning the
  # Manhattan distance lets us avoid calculating it multiple times.
  def parse(lines)
    lines.map do |line|
      line =~ /Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/
      sensor = Point.new(::Regexp.last_match(1).to_i, ::Regexp.last_match(2).to_i)
      beacon = Point.new(::Regexp.last_match(3).to_i, ::Regexp.last_match(4).to_i)
      [sensor, beacon, sensor.manhattan_distance(beacon)]
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2022, 15)
end
