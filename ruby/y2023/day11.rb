#!/usr/bin/env ruby
#
# Cosmic Expansion

require_relative '../day'
require_relative '../map'
require_relative '../point'

class Day11 < Day
  def do_part1(lines)
    map = Map.new(lines)
    expand(map)
    galaxy_locs = find_galaxies(map)
    galaxy_locs.combination(2).map { |p0, p1| p0.manhattan_distance(p1) }.sum
  end

  def do_part2(lines)
    # TODO
  end

  private

  def expand(map)
    empty_row_ixs = []
    map.height.times do |ri|
      empty_row_ixs << ri if map.cells[ri].all? { _1 == '.' }
    end
    row_to_insert = Array.new(map.width, '.')
    empty_row_ixs.reverse.each do |ri|
      map.insert_row(ri, row_to_insert)
    end

    empty_col_ixs = []
    map.width.times do |ci|
      empty_col_ixs << ci if map.col_cells(ci).all? { |ch| ch == '.' }
    end
    col_to_insert = Array.new(map.height.dup, '.')
    empty_col_ixs.reverse.each do |ci|
      map.insert_col(ci, col_to_insert)
    end
  end

  def find_galaxies(map)
    locs = []
    map.each do |ri, ci, ch|
      locs << Point.new(ri, ci) if ch == '#'
    end
    locs
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
