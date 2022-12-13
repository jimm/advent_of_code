#!/usr/bin/env ruby
#
# Rope Bridge

require 'set'
require_relative '../day'
require_relative '../point'

class Day09 < Day
  DIR_TO_SYM = { 'U' => :up, 'D' => :down, 'L' => :left, 'R' => :right }

  # "H" is head (leader) movement, "T" is follower
  # As of part 2, moves of leader also may be to "*"
  #
  # * H H H *
  # H . . . H
  # H . T . H
  # H . . . H
  # * H H H *

  FOLLOWER_FOLLOW_MOVES = {
    # key = head x, y diff, val = tail x, y move
    [-1, 2] => [-1, 1],
    [0,  2] => [0,  1],
    [1,  2] => [1,  1],

    [-2, 1] => [-1, 1],
    [2, 1] => [1, 1],
    [-2, 0] => [-1, 0],
    [2, 0] => [1, 0],
    [-2, -1] => [-1, -1],
    [2, -1] => [1, -1],

    [-1, -2] => [-1, -1],
    [0, -2] => [0, -1],
    [1, -2] => [1, -1],

    # corners for part 2
    [-2, 2] => [-1, 1],
    [2, 2] => [1, 1],
    [-2, -2] => [-1, -1],
    [2, -2] => [1, -1]
  }

  def part1
    puts do_part(2, data_lines(1))
  end

  def part1_tests
    do_tests(2)
  end

  def part2
    puts do_part(10, data_lines(1))
  end

  def part2_tests
    do_tests(10)
  end

  private

  def do_tests(num_knots)
    run_chunk_tests(@part_number) do |expected, lines|
      expected = expected.to_i
      answer = do_part(num_knots, lines)
      [answer == expected, answer, expected]
    end
  end

  def do_part(num_knots, lines)
    knots = (0...num_knots).map { Point.new }
    tail_visited = Set.new
    tail_visited.add(knots[-1].to_a)
    lines.each do |line|
      dir, dist = line.split
      dir = DIR_TO_SYM[dir]
      dist = dist.to_i
      dist.times do
        move(knots[0], dir)
        (1...num_knots).each { |i| follow(knots[i - 1], knots[i]) }
        tail_visited.add(knots[-1].to_a)
      end
    end
    tail_visited.size
  end

  def move(point, dir)
    case dir
    when :up
      point.y += 1
    when :down
      point.y -= 1
    when :left
      point.x -= 1
    when :right
      point.x += 1
    end
  end

  def follow(leader, follower)
    dx = leader.x - follower.x
    dy = leader.y - follower.y
    move = FOLLOWER_FOLLOW_MOVES[[dx, dy]]
    if move
      follower.x += move[0]
      follower.y += move[1]
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2022, 9)
end
