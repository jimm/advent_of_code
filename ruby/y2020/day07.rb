#!/usr/bin/env ruby
#
# Handy Haversacks

require 'set'
require_relative '../day'

class Day07 < Day
  MY_BAG = 'shiny gold'

  def part1
    parsed = data_lines(1).map { |line| parse_line(line) }
    rules = rules_from_parsed(parsed)

    # invert the rules
    inverted = {}
    rules.each_key do |bag|
      rules[bag].each_key do |contained_bag|
        inverted[contained_bag] ||= []
        inverted[contained_bag] << bag
      end
    end

    # Find bags that may contain my bag, any level deep
    start = MY_BAG
    set = Set.new
    appears_in = Set.new(inverted[start]) - set
    until appears_in.empty?
      set += appears_in
      appears_in = Set.new(appears_in
                             .map { |bag| inverted[bag] }
                             .flatten
                             .compact) - set
    end
    puts(set.length)
  end

  def part2
    puts(do_part2(@part_number))
  end

  # expect 32 and 126
  def part2_tests
    [1, 2].each do |part_num|
      puts(do_part2(part_num))
    end
  end

  def do_part2(part_num)
    @memoized_bags_inside = {}
    parsed = data_lines(part_num).map { |line| parse_line(line) }
    rules = rules_from_parsed(parsed)
    bags_inside(rules, MY_BAG)
  end

  def bags_inside(rules, bag, acc = 0)
    return acc + @memoized_bags_inside[bag] if @memoized_bags_inside.has_key?(bag)

    unless rules.has_key?(bag)
      @memoized_bags_inside[bag] = 0
      return acc
    end

    retval = rules[bag].keys.map do |contained_bag|
      num_contained = rules[bag][contained_bag]
      num_contained + num_contained * bags_inside(rules, contained_bag, acc)
    end
                       .reduce(:+)
    retval = 0 if retval.nil?
    @memoized_bags_inside[bag] = retval
    acc + retval
  end

  # Rules are in a dict whose keys are bags and values are dictionaries of
  # {bag name => number}
  def rules_from_parsed(parsed_lines)
    d = {}
    parsed_lines.each do |container, contents|
      d[container] ||= {}
      contents.each do |c|
        d[container][c[1]] = c[0] if c[1]
      end
    end
    d
  end

  def parse_line(line)
    line =~ /^(\w+ \w+) bags contain (.*)\.$/
    container = ::Regexp.last_match(1)
    contents = ::Regexp.last_match(2)
    contents = contents.split(', ').map do |content|
      content =~ /(\d+) (\w+ \w+) bags?/
      if ::Regexp.last_match(1).nil?
        []
      else
        [::Regexp.last_match(1).to_i, ::Regexp.last_match(2)]
      end
    end
    [container, contents]
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc
end
