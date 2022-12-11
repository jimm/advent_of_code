#!/usr/bin/env ruby
#
# Extended Polymerization

require_relative '../day'

class Day14 < Day
  def part1
    do_part(10)
  end

  def part2
    do_part(40)
  end

  def zero_hash
    Hash.new { |h, k| h[k] = 0 }
  end

  def do_part(steps)
    lines = data_lines(1)
    polymer_template = lines.shift
    rules = {}
    lines.each do |line|
      line =~ /(..) -> (.)/
      rules[Regexp.last_match(1)] = Regexp.last_match(2)
    end
    data = zero_hash
    polymer_template.split('').each_cons(2) { |a, b| data[a + b] += 1 }
    steps.times do |_|
      data = apply_rules(data, rules)
    end
    puts score(data)
  end

  def apply_rules(data, rules)
    new_data = zero_hash
    data.each do |key, val|
      new_data[key[0] + rules[key]] += val
      new_data[rules[key] + key[1]] += val
    end
    new_data
  end

  def score(data)
    letter_freqs = zero_hash
    data.each do |key, val|
      letter_freqs[key[0]] += val
      letter_freqs[key[1]] += val
    end

    # Each letter is counted twice except the ends. The (x+1)/2 compensates
    # for that.
    min_len = (letter_freqs.values.min + 1) / 2
    max_len = (letter_freqs.values.max + 1) / 2
    max_len - min_len
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc
end
