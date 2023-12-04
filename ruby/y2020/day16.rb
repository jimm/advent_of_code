#!/usr/bin/env ruby
#
# Ticket Translation

require_relative '../day'

class Rule
  attr_reader :name, :range1, :range2

  def initialize(name, min1, max1, min2, max2)
    @name = name
    @range1 = (min1..max1)
    @range2 = (min2..max2)
  end

  def valid?(val)
    @range1.include?(val) || @range2.include?(val)
  end
end

class Ticket
  attr_reader :nums

  def initialize(nums)
    @nums = nums
  end

  # Returns true if every rule is valid for at least one value in this
  # ticket.
  def valid?(rules)
    invalid_values(rules).empty?
  end

  # Returns all rules that are not valid for this ticket.
  def invalid_values(rules)
    if @invalid_values.nil?
      @invalid_values = @nums.select do |num|
        rules.none? { |rule| rule.valid?(num) }
      end
    end
    @invalid_values
  end
end

# A ticket is an array of integers.
class Day16 < Day
  def part1
    rules, my_ticket, other_tickets = parse_data
    invalid_tickets = other_tickets.reject { |t| t.valid?(rules) }
    ticket_scanning_error_rate =
      invalid_tickets.sum { |t| t.invalid_values(rules).sum }
    puts(ticket_scanning_error_rate)
  end

  def part2
    rules, my_ticket, other_tickets = parse_data
    other_tickets.select! { |t| t.valid?(rules) }

    possible_indexes = possible_rule_indexes(rules, other_tickets)
    ticket_field_indexes = reduce_rule_indexes(possible_indexes)

    answer = 1
    ticket_field_indexes.each_with_index do |ticket_field_index, rule_index|
      rule = rules[rule_index]
      if @testing
        puts("#{rule.name}: #{my_ticket.nums[ticket_field_index]}")
      elsif rule.name =~ /^departure/
        answer *= my_ticket.nums[ticket_field_index]
      end
    end
    puts(answer) unless @testing
  end

  # For each rule, determine which field indexes are those for which the
  # rule is valid for all tickets. Return an array of arrays.
  def possible_rule_indexes(rules, tickets)
    num_fields = rules.length
    rules.map do |rule|
      indexes = []
      (0...num_fields).each do |field_index|
        indexes << field_index if tickets.all? { |t| rule.valid?(t.nums[field_index]) }
      end
      indexes
    end
  end

  # Take array of arrays of possible indexes. Recursively eliminate
  # singleton entries from all other entries until they're all singletons,
  # then return those values.
  def reduce_rule_indexes(indexes)
    return indexes.map(&:first) if indexes.all? { |xs| xs.length == 1 }

    single_values = indexes.select { |xs| xs.length == 1 }.map(&:first)
    indexes = indexes.map { |xs| xs.length == 1 ? xs : xs - single_values }
    reduce_rule_indexes(indexes)
  end

  def parse_data
    rules = []
    my_ticket = nil
    other_tickets = []
    ticket_type = nil
    data_lines.each do |line|
      case line
      when /([\w\s]+): (\d+)-(\d+) or (\d+)-(\d+)/
        rules << Rule.new(::Regexp.last_match(1), ::Regexp.last_match(2).to_i, ::Regexp.last_match(3).to_i,
                          ::Regexp.last_match(4).to_i, ::Regexp.last_match(5).to_i)
      when /your ticket:/
        ticket_type = :mine
      when /nearby tickets:/
        ticket_type = :other
      else
        nums = line.split(',').map(&:to_i)
        if ticket_type == :mine
          my_ticket = Ticket.new(nums)
        else
          other_tickets << Ticket.new(nums)
        end
      end
    end
    [rules, my_ticket, other_tickets]
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
