#!/usr/bin/env ruby
#
# Boiling Boulders

require 'set'
require_relative '../day'
require_relative '../point'

class Day18 < Day
  class Cube
    attr_reader :corner, :faces

    def initialize(corner)
      @corner = corner
      @vertices = generate_vertices
      @faces = generate_faces
    end

    def generate_vertices
      [
        @corner,
        Point.new(@corner.x + 1, @corner.y,     @corner.z),
        Point.new(@corner.x + 1, @corner.y + 1, @corner.z),
        Point.new(@corner.x,     @corner.y + 1, @corner.z),
        Point.new(@corner.x,     @corner.y,     @corner.z + 1),
        Point.new(@corner.x + 1, @corner.y,     @corner.z + 1),
        Point.new(@corner.x + 1, @corner.y + 1, @corner.z + 1),
        Point.new(@corner.x,     @corner.y + 1, @corner.z + 1)
      ]
    end

    # Returns six four-element arrays of vertices. Each face's list of
    # vertices is sorted.
    def generate_faces
      [
        [@vertices[0], @vertices[1], @vertices[2], @vertices[3]].sort,
        [@vertices[4], @vertices[5], @vertices[6], @vertices[7]].sort,

        [@vertices[0], @vertices[1], @vertices[4], @vertices[5]].sort,
        [@vertices[2], @vertices[3], @vertices[6], @vertices[7]].sort,

        [@vertices[0], @vertices[3], @vertices[4], @vertices[7]].sort,
        [@vertices[1], @vertices[2], @vertices[5], @vertices[6]].sort
      ]
    end
  end

  def part1
    puts do_part(data_lines(1))
  end

  def part1_tests
    do_tests
  end

  def part2
    puts do_part(data_lines(1))
  end

  def part2_tests
    do_tests
  end

  private

  def do_tests
    run_chunk_tests(1) do |expected, lines|
      expected = expected.split(',')[@part_number - 1].to_i
      answer = send(:"do_part#{@part_number}".to_sym, lines)
      [answer == expected, answer, expected]
    end
  end

  def do_part1(lines)
    cubes = parse(lines)
    total_faces = cubes.length * 6
    showing_faces = Set.new(cubes.flat_map(&:faces)).size
    total_faces - (total_faces - showing_faces) * 2
  end

  def do_part2(lines)
    # TODO
  end

  def parse(lines)
    lines.map do |line|
      coords = line.split(',').map(&:to_i)
      Cube.new(Point.new(*coords))
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2022, 18)
end
