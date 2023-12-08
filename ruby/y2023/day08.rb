#!/usr/bin/env ruby
#
# Haunted Wasteland

require_relative '../day'
require_relative '../math'

class Day08 < Day
  def do_part1(lines)
    directions, nodes = parse(lines)
    steps_to_z('AAA', directions, nodes, ->(n) { n == 'ZZZ' })
  end

  def do_part2(lines)
    directions, nodes = parse(lines)
    start_nodes = nodes.keys.select { _1[-1] == 'A' }
    steps = start_nodes.map do |start_node|
      steps_to_z(start_node, directions, nodes, ->(n) { n[-1] == 'Z' })
    end
    lcm(steps)
  end

  # Block defines how to identify an end node
  def steps_to_z(start_node, directions, nodes, end_node_lambda)
    steps = 0
    curr_node = start_node
    until end_node_lambda.call(curr_node)
      direction = directions[steps % directions.length]
      curr_node = nodes[curr_node][direction]
      steps += 1
    end
    steps
  end

  private

  def parse(lines)
    directions = lines[0].split('').map { _1 == 'L' ? 0 : 1 }
    nodes = {}
    lines[1..].each do |line|
      line =~ /(\w+) = \((\w+), (\w+)/
      name = ::Regexp.last_match(1)
      left = ::Regexp.last_match(2)
      right = ::Regexp.last_match(3)
      nodes[name] = [left, right]
    end
    [directions, nodes]
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
