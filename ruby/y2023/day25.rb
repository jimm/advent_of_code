#!/usr/bin/env ruby
#
# Snowverload

require_relative '../day'
require_relative 'graph'

class Day25 < Day
  def do_part1(lines)
    graph = parse(lines)
  end

  def do_part2(lines)
    # TODO
  end

  def parse(lines)
    graph = Graph.new
    lines.each do |line|
      left, others = line.split(': ')
      left = find_or_create_vertex(graph, left)
      others = others.split(' ').map { find_or_create_vertex(graph, _1) }
      others.each do |v|
        graph.add_edge(left, v)
      end
    end
    graph
  end

  def find_or_create_vertex(graph, name)
    v = graph.vertices.detect { _1.name == name }
    return v if v

    v = Vertex.new(name)
    graph.vertices << v
    v
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
