#!/usr/bin/env ruby
#
# Proboscidea Volcanium

require_relative '../day'

class Day16 < Day
  # ================ Node ================

  class Node
    @@nodes = []

    attr_reader :name, :rate, :nodes

    def self.nodes
      @@nodes
    end

    def initialize(name, rate)
      @name = name
      @rate = rate
      @nodes = []
      @@nodes << self
    end

    def to_s
      "Node<name=#{@name}, rate=#{@rate}, nodes=#{@nodes.map(&:name)}>"
    end

    def inspect
      to_s
    end
  end

  # ================ State ================

  # State is immutable. Use #modified to get new instances.
  class State
    attr_reader :node, :minute, :total_flow_rate, :open_nodes, :num_openable_nodes, :already_visited

    def initialize(node)
      @node = node
      @minute = 0
      @total_flow_rate = 0
      @open_nodes = []
      @num_openable_nodes = Node.nodes.select { |n| n.rate > 0 }.length
      @already_visited = []
    end

    # Returns a new state that is a dup of this one with ivars modified as
    # in `h`.
    def modified(h)
      state = dup
      h.each { |k, v| state.instance_variable_set("@#{k}".to_sym, v) }
      puts state # DEBUG
      state
    end

    def this_minute_flow_rate
      @this_minute_flow_rate ||= @open_nodes.map(&:rate).sum
    end

    def already_visited?(target)
      @already_visited.include?([@node, target])
    end

    def can_open_current_node?
      @node.rate > 0 && !@open_nodes.include?(@node)
    end

    def all_nodes_open?
      @open_nodes.length == @num_openable_nodes
    end

    def to_s
      "State<minute=#{minute}, node=#{@node.name}>, total_flow_rate=#{@total_flow_rate}, open_nodes=#{open_nodes.map(&:name)}"
    end

    def inspect
      to_s
    end
  end

  # ================ Day16 ================

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
      answer = do_part(lines)
      [answer == expected, answer, expected]
    end
  end

  def do_part(lines)
    root = parse(lines)
    multiverse_traverse(State.new(root))
  end

  def multiverse_traverse(state)
    return state.total_flow_rate if state.minute >= 30
    return state.total_flow_rate + state.this_minute_flow_rate * (30 - state.minute) if state.all_nodes_open?

    possibilities = []
    new_flow_rate = state.total_flow_rate + state.this_minute_flow_rate

    # open current node if it has a positive flow rate
    if state.can_open_current_node?
      new_state = state.modified(
        minute: state.minute + 1,
        total_flow_rate: new_flow_rate + state.node.rate,
        open_nodes: state.open_nodes + [state.node]
      )
      possibilities << multiverse_traverse(new_state)
    end

    # move
    state.node.nodes.each do |target|
      next if state.already_visited?(target)

      new_state = state.modified(
        node: target,
        minute: state.minute + 1,
        total_flow_rate: new_flow_rate,
        already_visited: state.already_visited + [[state.node, target]]
      )
      possibilities << multiverse_traverse(new_state)
    end

    (possibilities + [new_flow_rate]).max
  end

  def parse(lines)
    graph = {}
    lines.each do |line|
      line =~ /Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.*)/
      name = ::Regexp.last_match(1)
      rate = ::Regexp.last_match(2).to_i
      targets = ::Regexp.last_match(3).split(', ')
      graph[name] = [rate, targets]
    end

    debug_graph(graph)
    exit 0

    # now turn graph into nodes
    temp_lookup = {}
    nodes = graph.map { |k, v| temp_lookup[k] = Node.new(k, v[0]) }

    graph.each do |k, v|
      node = temp_lookup[k]
      v[1].each { |name| node.nodes << temp_lookup[name] }
    end
    temp_lookup['AA']
  end

  def debug_graph(graph)
    puts 'digraph G {'
    graph.keys.each do |name|
      rate = graph[name][0]
      if rate > 0
        puts "  #{name} [label=\"#{name} - #{graph[name][0]}\" style=\"filled\" color=\"salmon\"]"
      else
        puts "  #{name} [label=\"#{name} - #{graph[name][0]}\"]"
      end
    end
    graph.each do |name, val|
      val[1].each { |target| puts "  #{name} -> #{target}" }
    end
    puts '}'
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
