#!/usr/bin/env ruby
#
# Aplenty

require_relative '../day'

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
    def initialize(part_field, comparitor, value)
      @part_field = part_field.to_sym
      @comparitor = comparitor.to_sym
      @value = value.to_i
    end

    def pass?(part)
      part.send(@part_field).send(@comparitor, @value)
    end

    def to_s
      "Condition(#{@field} #{@comparitor} #{@value})"
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
      @workflows = {}
      workflows.each { |w| @workflows[w.name] = w }
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

    def accepted_ranges(ranges)
      do_accepted_ranges([@workflows[:in]], ranges)
    end

    def do_accepted_ranges(workflows, ranges)
      return ranges if workflows.empty?
      # TODO
    end
  end

  def do_part1(lines)
    processor, parts = parse(lines)
    parts.select { |p| processor.accepted?(p) }.sum(&:score)
  end

  def do_part2(lines)
    processor, _ = parse(lines)
    ranges = processor.accepted_ranges(
      {x: [(1..4000)], m: [(1..4000)], a: [(1..4000)], s: [(1..4000)]}
    )
    ranges[:x].sum(&:size) * ranges[:m].sum(&:size) *
      ranges[:a].sum(&:size) * ranges[:s].sum(&:size)
  end

  private

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
    [PartProcessor.new(workflows), parts]
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
