#!/usr/bin/env ruby
#
# Christmas Tree Farm

require_relative '../day'
require_relative '../map'
require_relative '../point'

class Day12 < Day
  class Shape
    attr_reader :width, :height, :coords

    def initialize(lines)
      @width = lines[0].length
      @height = lines.length
      @coords = Set.new
      lines.each_with_index do |line, row|
        line.chars.each_with_index do |ch, col|
          @coords << Point.new(col, row) if ch == '#'
        end
      end
    end

    # Returns all possible orientations of shape.
    def orientations
      @orientations ||= generate_orientations
    end

    # Returns the [max_row, max_col] for which this shape can still fit the
    # given `height` and `width`.
    def max_row_and_col_for(height, width)
      [height - @height, width - @width]
    end

    def to_s
      "#{@width}x#{@height}: #{@coords.inspect}"
    end

    private

    # Returns an array of all possible orientations of our coords.
    def generate_orientations
      orientations = Set.new
      coords = @coords.dup
      2.times do
        2.times do
          4.times do
            new_coords = rotate(coords)
            orientations << new_coords
            coords = new_coords
          end
          coords = flip(coords)
        end
        coords = invert(coords)
      end
      puts "num orientations generated = #{orientations.length}" if $DEBUG

      orientations
    end

    # Returns coords rotated 90 degrees clockwise.
    def rotate(coords)
      new_coords = Set.new
      coords.each { |coord| new_coords << Point.new(coord.y, @width - coord.x - 1) }
      new_coords
    end

    # Returns coords, left-right flipped.
    def flip(coords)
      new_coords = Set.new
      coords.each { |coord| new_coords << Point.new(coord.x, @width - coord.y - 1) }
      new_coords
    end

    # Returns coords, top-bottom inverted.
    def invert(coords)
      new_coords = Set.new
      coords.each { |coord| new_coords << Point.new(@width - coord.x - 1, coord.y) }
      new_coords
    end
  end

  class Tree
    attr_reader :width, :height, :present_quantities

    def initialize(width, height, present_quantities)
      @width = width
      @height = height
      @present_quantities = present_quantities
    end

    # Returns true if we can fit the desired `@present_quantities` using
    # `shapes`, which can be rotated and flipped.
    #
    # We start with an empty set of coords. For each shape, we try all
    # possible orientations that fit N times, then move on to the next shape
    # for each orientation.
    def can_fit_presents?(shapes)
      puts('**** can_fit_presents?')
      do_can_fit_presents?(shapes, @present_quantities, Set.new)
    end

    def do_can_fit_presents?(shapes, desired_quantities, occupied_coords)
      return true if desired_quantities.all?(&:zero?)

      # Find a shape that must be fit
      shape_idx = desired_quantities.index { |count| count > 0 }
      shape = shapes[shape_idx]
      max_row, max_col = shape.max_row_and_col_for(@height, @width)

      # For the next iteration, reduce the number of presents needed with this shape
      new_desired_quantities = desired_quantities.dup
      new_desired_quantities[shape_idx] -= 1

      # Iterate over all of the possible starting upper-left corners.
      offset = Point.new
      0.upto(max_row) do |row|
        offset.y = row
        0.upto(max_col) do |col|
          offset.x = col

          # Iterate over all orientations, finding the ones that fit. For
          # each of those, continue recursing to fit the remaining presents
          # required.
          shape
            .orientations
            .select { |coords| can_fit?(coords, offset, occupied_coords) }
            .each do |coords|
              fit_offset_coords = coords.map { |c| c + offset }
              new_occupied_coords = occupied_coords.dup + fit_offset_coords
              return true if do_can_fit_presents?(shapes, new_desired_quantities, new_occupied_coords)
            end
        end
      end
      false
    end

    # Returns true if all `coords` can fit in `occupied_coords` at `offset`.
    def can_fit?(coords, offset, occupied_coords)
      offset_coords = coords.map { |c| c + offset }
      (occupied_coords & offset_coords).empty?
    end

    def to_s
      "#{@width}x#{@height}: #{@present_quantities.inspect}"
    end
  end

  def do_part1(lines)
    shapes, trees = parse(lines)
    trees
      .select { |t| t.can_fit_presents?(shapes) }
      .length
  end

  def do_part2(lines)
    # TODO
  end

  # FIXME: last shape isn't getting created

  # Returns a two-element aray of [[shape maps], [trees]] where "tree" is
  # [[w, h, shape_nums]].
  def parse(lines)
    shapes = []
    shape = []
    trees = []
    shape_index = -1
    lines.each do |line|
      case line
      when /^(\d+)x(\d+): (.*)/
        shapes[shape_index] = Shape.new(shape)
        w = ::Regexp.last_match(1).to_i
        h = ::Regexp.last_match(2).to_i
        shape_nums = ::Regexp.last_match(3).split(' ').map(&:to_i)
        trees << Tree.new(w, h, shape_nums)
      when /^(\d+):/
        shapes[shape_index] = Shape.new(shape) if shape_index >= 0
        shape_index = ::Regexp.last_match(1).to_i
        shape = []
      when /^[#.]{3}/
        shape << line
      end
    end
    [shapes, trees]
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
