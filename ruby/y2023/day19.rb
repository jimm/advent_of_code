#!/usr/bin/env ruby
#
# Aplenty

require_relative '../day'
require_relative 'graph'

class Day19 < Day
  class Part
    attr_reader :x, :m, :a, :s

    def initialize(x, m, a, s)
      @x = x
      @m = m
      @a = a
      @s = s
    end

    def score = @x + @m + @a + @s

    def to_s = "Part(x=#{@x},m=#{@m},a=#{@a},s=#{@s})"
  end

  class Condition
    attr_reader :part_field, :comparitor, :value
    def initialize(part_field, comparitor, value)
      @part_field = part_field.to_sym
      @comparitor = comparitor.to_sym
      @value = value.to_i
    end

    def pass?(part)
      part.send(@part_field).send(@comparitor, @value)
    end

    def to_s
      "Condition(#{@part_field} #{@comparitor} #{@value})"
    end
  end

  class Rule
    attr_reader :condition, :destination

    def initialize(condition, destination)
      @condition = condition
      @destination = case destination
                     when 'R'
                       :rejected
                     when 'A'
                       :accepted
                     else
                       destination.to_sym
                     end
    end

    def pass?(_part)
      raise 'subclasses must implement'
    end

    def to_s
      "Rule(condition=#{@condition}, destination=#{@destination}"
    end
  end

  class ConditionalRule < Rule
    attr_reader :condition
    def pass?(part)
      @condition.pass?(part)
    end
  end

  class UnconditionalRule < Rule
    def initialize(destination)
      super(nil, destination)
    end

    def pass?(_part)
      true
    end
  end

  class Workflow
    attr_reader :name, :rules

    def initialize(name, rules)
      @name = name.to_sym
      @rules = rules
    end

    # Given a part, apply all of our conditions and return :rejected,
    # :accepted, or the name of the next workflow to apply.
    def apply_to(part)
      rules.each do |rule|
        return rule.destination if rule.pass?(part)
      end
    end

    def to_s = "Workflow(name=#{@name})"
  end

  class PartProcessor
    attr_reader :workflows

    def initialize(workflows)
      @workflows = workflows
    end

    def accepted?(part)
      workflow = @workflows[:in]
      while true
        next_sym = workflow.apply_to(part)
        return true if next_sym == :accepted
        return false if next_sym == :rejected
        workflow = @workflows[next_sym]
      end
    end
  end

  class WorkflowVertex < Vertex
    attr_reader :workflow
    def initialize(workflow)
      super(workflow == :accepted || workflow == :rejected ? workflow.to_s : workflow.name.to_s)
      @workflow = workflow
    end

    def accepted? = @workflow == :accepted
    def rejected? = @workflow == :rejected
    def start? = @workflow.name == :in
    def end? = accepted? || rejected?

    def name = end? ? @workflow : @workflow.name

    def to_s = "WorkflowVertex(name=#{name})"
  end

  class WorkflowEdge < Edge
    attr_reader :source, :rule, :dest
    def initialize(source, rule, dest)
      super(source, dest)
      @rule = rule
    end
  end

  def do_part1(lines)
    workflows, parts = parse(lines)
    processor = PartProcessor.new(workflows)
    parts.select { |p| processor.accepted?(p) }.sum(&:score)
  end

  def do_part2(lines)
    workflows, _ = parse(lines)
    graph = build_graph(workflows)
    accepted = graph.vertices.detect(&:accepted?)
    flows = vertices_from_start_to(accepted)
              .flatten
              .slice_before(&:start?)

    ranges = %i(x m a s).zip(%i(x m a s).map { (1..4000) }).to_h
    ranges = accepted_ranges(flows, ranges)
               .pdebug

    # here's the brute force part
    count = 0
    (1...4000).each do |x|
      (1...4000).each do |m|
        (1...4000).each do |a|
          (1...4000).each do |s|
            if ranges.any? do |range|
                 range[:x].include?(x) &&
                   range[:m].include?(m) &&
                   range[:a].include?(a) &&
                   range[:s].include?(s)
               end
              count += 1
            end
          end
        end
      end
    end
  end

  private

  # Given a vertex, return an array of arrays of vertices that lead from the
  # start node to this vertex.
  def vertices_from_start_to(vertex)
    return [vertex] if vertex.start?
    vertex
      .inputs
      .map { |edge| vertices_from_start_to(edge.source) }
      .map { |arr| arr << vertex }
  end

  def accepted_ranges(flows, ranges)
    rs = []
    flows.each do |flow|
      flow_range = ranges.dup
      flow.each_cons(2) do |v0, v1|
        rule = v0.workflow.rules.detect { _1.destination == v1.name }
        if rule.kind_of?(ConditionalRule)
          cond = rule.condition
          range = flow_range[cond.part_field]
          case cond.comparitor
          when :>
            new_min = [cond.value, range.min].max
            range = (new_min..range.max)
          when :<
            new_max = [cond.value, range.max].min
            range = (range.min..new_max)
          end
          flow_range[cond.part_field] = range
        end
      end
      if %i(x m a s).none? { flow_range[_1].size == 0 }
        rs << flow_range
      end
    end
    rs
  end

  def apply_node_to_ranges(node, ranges)
    cond = node.rule.condition
    if cond
      ranges[cond.part_field] = remove_from_range(ranges[cond.part_field], cond)
    end
  end

  # Returns a Graph.
  def build_graph(workflows)
    graph = Graph.new
    graph.add_vertex(WorkflowVertex.new(:accepted))
    graph.add_vertex(WorkflowVertex.new(:rejected))

    workflows.each do |name, workflow|
      graph.add_vertex(WorkflowVertex.new(workflow))
    end
    workflows.each do |name, workflow|
      workflow.rules.each do |rule|
        graph.add_edge(
          graph.vertices.detect { _1.name == name },
          graph.vertices.detect { _1.name == rule.destination }
        )
      end
    end
    graph
  end

  def parse(lines)
    parts = []
    workflows = []
    
    lines.each do |line|
      case line[0]
      when nil
        # skip blank line
      when '{'
        line =~ /{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}/
        parts << Part.new($1.to_i, $2.to_i, $3.to_i, $4.to_i)
      else
        line =~ /(\w+){(.*)}/
        name = $1
        rules = $2.split(',').map do |rule|
          a, b = rule.split(':')
          if b
            ConditionalRule.new(Condition.new(a[0], a[1], a[2..]), b)
          else
            UnconditionalRule.new(a)
          end
        end
        workflows << Workflow.new(name, rules)
      end
    end
    workflows_hash = {}
    workflows.each { |w| workflows_hash[w.name] = w }
    [workflows_hash, parts]
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
