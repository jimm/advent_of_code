#!/usr/bin/env ruby
#
# The Floor Will Be Lava

require 'set'
require_relative '../day'
require_relative '../map'
require_relative '../point'

class Day16 < Day
  class Beam < Point
    attr_accessor :dir # :left, :right, :up, :down

    def initialize(x = 0, y = 0, dir)
      super(x, y, 0)
      @dir = dir
    end

    def hash
      [@x, @y, @dir].hash
    end

    def ==(other)
      super(other) && @dir == other.dir
    end

    def to_s
      "(#{@x}, #{@y}, #{@z}, #{@dir})"
    end

    def inspect
      "Beam#{self}"
    end

    def seen_key
      [@x, @y, @dir]
    end
  end

  def do_part1(lines)
    map = Map.new(lines)
    energized_tiles(map)
  end

  def do_part2(lines)
    # TODO
  end

  private

  def energized_tiles(map)
    beams = [Beam.new(0, -1, :right)]
    energized = Set.new
    # optimization: if beam gets to a tile that is already energized with a
    # beam with the same loc and dir, we can stop.
    seen = Set.new # [x, y, direction]

    until beams.empty?
      beam = beams.shift
      follow_path(map, beam, beams, seen, energized)
    end

    energized.size
  end

  def follow_path(map, beam, beams, seen, energized)
    while true
      move(map, beam)
      return unless map.in_bounds?(beam.x, beam.y)
      return if seen.include?(beam.seen_key)

      seen.add(beam.seen_key)
      energized.add([beam.x, beam.y])
      case map.at(beam.x, beam.y)
      when '.'
        # nop
      when '/'
        beam.dir = {
          left: :down,
          right: :up,
          up: :right,
          down: :left
        }[beam.dir]
      when '\\'
        beam.dir = {
          left: :up,
          right: :down,
          up: :left,
          down: :right
        }[beam.dir]
      when '|'
        if beam.dir == :right || beam.dir == :left
          if beam.x == 0
            beam.dir = :down
          elsif beam.x == map.height - 1
            beam.dir = :up
          else
            beams << Beam.new(beam.x, beam.y, :up)
            beams << Beam.new(beam.x, beam.y, :down)
            return
          end
        end
      when '-'
        if beam.dir == :up || beam.dir == :down
          if beam.y == 0
            beam.dir = :right
          elsif beam.y == map.width - 1
            beam.dir = :left
          else
            beams << Beam.new(beam.x, beam.y, :left)
            beams << Beam.new(beam.x, beam.y, :right)
            return
          end
        end
      end
    end
  end

  def move(map, beam)
    case beam.dir
    when :left
      beam.y -= 1
    when :right
      beam.y += 1
    when :up
      beam.x -= 1
    when :down
      beam.x += 1
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
