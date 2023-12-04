#!/usr/bin/env ruby
#
# Rain Risk

require_relative '../day'

class Ship
  HEADING_TO_DELTA = {
    north: [0, 1],
    south: [0, -1],
    east: [1, 0],
    west: [-1, 0]
  }
  HEADING_TURNS = {
    [:north, 'L'] => %i[west south east north],
    [:north, 'R'] => %i[east south west north],
    [:south, 'L'] => %i[east north west south],
    [:south, 'R'] => %i[west north east south],
    [:east, 'L'] => %i[north west south east],
    [:east, 'R'] => %i[south west north east],
    [:west, 'L'] => %i[south east north west],
    [:west, 'R'] => %i[north east south west]
  }

  def initialize
    @x = @y = 0
    @wx = 10 # waypoint
    @wy = 1
    @heading = :east
  end

  def move(move_str, move_func)
    instruction = move_str[0]
    distance = move_str[1..-1].to_i
    send(move_func, instruction, distance)
  end

  def simple_move(instruction, distance)
    case instruction
    when 'F'
      @x += HEADING_TO_DELTA[@heading][0] * distance
      @y += HEADING_TO_DELTA[@heading][1] * distance
    when 'N'
      @y += distance
    when 'S'
      @y -= distance
    when 'E'
      @x += distance
    when 'W'
      @x -= distance
    when 'L', 'R'
      moves = HEADING_TURNS[[@heading, instruction]].cycle
      n = distance / 90
      @heading = moves.take(n)[-1]
    end
  end

  def waypoint_move(instruction, distance)
    case instruction
    when 'F'
      @x += @wx * distance
      @y += @wy * distance
    when 'N'
      @wy += distance
    when 'S'
      @wy -= distance
    when 'E'
      @wx += distance
    when 'W'
      @wx -= distance
    when 'L', 'R'
      n = (distance % 360) / 90
      n_clockwise = n
      n_clockwise = 4 - n if instruction == 'L'
      n_clockwise.times do |_|
        if @wx >= 0             # right half
          if @wy >= 0           # upper right -> lower right
            @wx, @wy = @wy, -@wx
          else                  # lower right -> lower left
            @wx, @wy = @wy, -@wx
          end
        elsif @wx >= 0 # left half
          @wx, @wy = -@wy, @wx # upper left -> upper right
        else                  # lower left -> upper left
          @wx, @wy = @wy, -@wx
        end
      end
    end
  end

  def manhattan_distance_from_origin
    @x.abs + @y.abs
  end
end

class Day12 < Day
  def part1
    do_part(:simple_move)
  end

  def part2
    do_part(:waypoint_move)
  end

  def do_part(func_sym)
    ship = Ship.new
    data_lines(1).each { |line| ship.move(line, func_sym) }
    puts(ship.manhattan_distance_from_origin)
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
