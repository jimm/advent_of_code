#!/usr/bin/env ruby
#
# Universal Orbit Map

require_relative '../day'

class Day06 < Day
  # Orbits are stored as a hash where key = orbiting body and val = inner
  # body.

  def do_part1(lines)
    orbits = create_orbit_map(lines)
    orbits.keys.sum { |k| path_to_com_from(k, orbits).size }
  end

  def do_part2(lines)
    orbits = create_orbit_map(lines)
    start = orbits['YOU']
    target = orbits['SAN']

    my_path_to_com = path_to_com_from('YOU', orbits)
    santa_path_to_com = path_to_com_from('SAN', orbits)
    innermost = common_ancestor(my_path_to_com, santa_path_to_com)
    path = moves_in(my_path_to_com, innermost)
    path += moves_out(santa_path_to_com, innermost, target)

    path.size
  end

  private

  def create_orbit_map(lines)
    h = {}
    lines.each do |line|
      inner, outer = line.split(')')
      h[outer] = inner
    end
    h
  end

  # Returns path from `obj` to COM, not including `obj`.
  def path_to_com_from(obj, orbits)
    path = []
    while obj
      obj = orbits[obj]
      path << obj if obj
    end
    path
  end

  def common_ancestor(path1, path2)
    # O(n^2) but we don't care
    path1.each do |obj|
      return obj if path2.include?(obj)
    end
    'COM'
  end

  # Returns objs in between start (second item in path, first is "YOU") and
  # `to` exclusive.
  def moves_in(path, to)
    moves = path[1..].take_while { |obj| obj != to }
  end

  # Returns objs from `from` to `to` inclusive.
  def moves_out(path, from, to)
    moves = path
            .reverse
            .drop_while { |obj| obj != from }
            .take_while { |obj| obj != to }
    moves << to
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
