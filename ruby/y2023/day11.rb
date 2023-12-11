#!/usr/bin/env ruby
#
# Cosmic Expansion

require_relative '../day'
require_relative '../map'
require_relative '../point'

class Day11 < Day
  def do_part1(lines)
    do_part(lines, 2)
  end

  def do_part2(lines)
    do_part(lines, @testing ? 100 : 1_000_000)
  end

  def do_part(lines, expansion_factor)
    map = Map.new(lines)
    expanded_indexes = indexes_of_expanded(map)
    find_galaxies(map)
      .combination(2)
      .map { |g0, g1| expanded_distance(g0, g1, map, expansion_factor, expanded_indexes) }
      .sum
  end

  private

  # Returns a list with two elements: the list of expanded row indexes and
  # the list of expanded column indexes.
  def indexes_of_expanded(map)
    empty_row_ixs = []
    map.height.times do |ri|
      empty_row_ixs << ri if map.row(ri).all? { _1 == '.' }
    end

    empty_col_ixs = []
    map.width.times do |ci|
      empty_col_ixs << ci if map.column(ci).all? { |ch| ch == '.' }
    end

    [empty_row_ixs, empty_col_ixs]
  end

  def find_galaxies(map)
    locs = []
    map.each do |ri, ci, ch|
      locs << Point.new(ri, ci) if ch == '#'
    end
    locs
  end

  # Returns the manhattan distance between two points, after taking the
  # expansion of rows and columns between them into account.
  def expanded_distance(g0, g1, map, expansion_factor, expanded_indexes)
    row_steps = num_steps(g0.x, g1.x, expansion_factor, expanded_indexes[0])
    col_steps = num_steps(g0.y, g1.y, expansion_factor, expanded_indexes[1])
    row_steps + col_steps
  end

  def num_steps(n0, n1, expansion_factor, expanded_indexes)
    ns = [n0, n1].minmax
    n_range = (ns[0]..ns[1])
    num_expanded_steps = expanded_indexes.select { n_range.include?(_1) }.length
    step_diff = (ns[0] - ns[1]).abs
    step_diff + num_expanded_steps * (expansion_factor - 1)
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
