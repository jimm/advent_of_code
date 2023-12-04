#!/usr/bin/env ruby
#
# Grove Positioning System

require_relative '../day'

class Day20 < Day
  class RingNode
    attr_reader :value, :orig_index
    attr_accessor :left, :right

    def initialize(value, num_nodes)
      @value = value
      @num_nodes = num_nodes
    end

    def move
      distance = @value.abs % (@num_nodes - 1)
      return if distance == 0

      if @value > 0
        to_right_of = @left
        (distance + 1).times { to_right_of = to_right_of.right }
        to_left_of = to_right_of.right
      else
        to_left_of = @right
        (distance + 1).times { to_left_of = to_left_of.left }
        to_right_of = to_left_of.left
      end

      # remove from ring
      @left.right = @right
      @right.left = @left

      # insert into ring
      to_right_of.right = self
      @left = to_right_of
      to_left_of.left = self
      @right = to_left_of

      # puts "#{@value} moves between #{@left.value} and #{@right.value}" # DEBUG
    end
  end

  def part1
    puts do_part1(data_lines(1))
  end

  def part1_tests
    do_tests
  end

  def part2
    puts do_part2(data_lines(1))
  end

  def part2_tests
    do_tests
  end

  private

  def do_tests
    run_chunk_tests(1) do |expected, lines|
      expected = expected.split(',')[@part_number - 1].to_i
      answer = send(:"do_part#{@part_number}", lines)
      [answer == expected, answer, expected]
    end
  end

  def do_part1(lines)
    do_part(lines, 1, 1)
  end

  def do_part2(lines)
    do_part(lines, 811_589_153, 10)
  end

  def do_part(lines, multiply_by, num_mixes)
    seq = parse(lines).map { |n| n * multiply_by }
    len = seq.length

    zero_node = mix(seq, num_mixes)
    node = zero_node
    sum = 0
    3.times do |_|
      1000.times do |_|
        node = node.right
      end
      sum += node.value
    end
    sum
  end

  def parse(lines)
    lines.map(&:to_i)
  end

  def mix(seq, times = 1)
    nodes, zero_node = ring_from_seq(seq)
    times.times { nodes.each(&:move) }
    zero_node
  end

  # Creates a ring of RingNodes and returns an array containing the ring's
  # nodes in the same order as `seq`.
  def ring_from_seq(seq)
    len = seq.length
    zero_node = nil
    nodes = seq.each.map do |n|
      node = RingNode.new(n, len)
      zero_node = node if n == 0
      node
    end
    nodes.each_with_index do |node, i|
      node.right = nodes[(i + 1) % len]
      node.left = nodes[i - 1]
    end
    [nodes, zero_node]
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
