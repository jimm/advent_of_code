#!/usr/bin/env ruby
#
# Reactor

require_relative '../day'

class Day11 < Day
  def do_part1(lines)
    conns = parse_connections(lines)
    num_paths_from(:you, conns)
  end

  def do_part2(lines)
    conns = parse_connections(lines)
    paths_from(:svr, conns, [], [])
      .select { |path| path.include?(:dac) && path.include?(:fft) }
      .length
  end

  private

  # Returns number of paths from node to :out. Assumes no cycles.
  def num_paths_from(node, conns)
    return 1 if node == :out

    children = conns[node]
    children.map { |child| num_paths_from(child, conns) }.sum
  end

  # Returns all paths from node to :out. Assumes no cycles.
  def paths_from(node, conns, curr_path, paths)
    curr_path << node
    if node == :out
      paths << curr_path
      return
    end

    children = conns[node]
    children.each { |child| paths_from(child, conns, curr_path.dup, paths) }
    paths
  end

  def parse_connections(lines)
    conns = {}
    lines.map do |line|
      node, rest = line.split(': ')
      outputs = rest.split(' ')
      conns[node.to_sym] = outputs.map(&:to_sym)
    end
    conns
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
