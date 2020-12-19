# Monster Messages

require_relative '../utils'

class Day19 < Day
  def part1
    regex, inputs = parse(data_lines(1))
    puts(count_matches(regex, inputs))
  end

  def part1_tests
    do_tests
  end

  def part2
    regex, inputs = parse(data_lines(1), :part2_modify_rules)
  end

  def part2_modify_rules(index, rule)
    case index
    when 8
      # "42 | 42 8"
      "+ 42"
    when 11
      "42 31 | 42 11 31"
    else
      rule
    end
  end

  def part2_tests
    do_tests(:part2_modify_rules)
  end

  def do_tests(modify_rules_func=nil)
    run_chunk_tests(@part_number) do |expected, lines|
      regex, inputs = parse(lines, modify_rules_func)
      answer = count_matches(regex, inputs)
      [answer == expected.to_i, answer]
    end
  end

  def parse(lines, modify_rules_func=nil)
    rule_lines, inputs = lines.partition { |line| line =~ /:/ }
    # rules will end up being an array of either single letters or arrays of
    # "or" indexes. For example, "1 2" will become [[1, 2]] and "2 3 | 3 2"
    # will become [[2, 3], [3, 2]]
    rules = []
    rule_lines.each do |line|
      index, rule = line.split(': ')
      index = index.to_i

      if rule[0] == '"'
        rule = rule[1]
      else
        if modify_rules_func
          rule = send(modify_rules_func, index, rule)
        end
        rule = [:or] + rule.split('|').map do |num_text|
          nums = num_text.split(' ')
          if nums[0] == '+'
            # Handle special case "+ n n n n..." from rule 8 override
            [:+] + nums[1..-1].map(&:to_i)
          else
            # Concatenate
            [:cat] + nums.map(&:to_i)
          end
        end
      end
      rules[index.to_i] = rule
    end

    # # DEBUG
    # puts reduced_to_regex_str(reduce(rules, rules[42]))
    # puts reduced_to_regex_str(reduce(rules, rules[31]))
    # return [//, []]

    reduced = reduce(rules)
    regex_str = reduced_to_regex_str(reduced)
    regex = /\A#{regex_str}\z/
    [regex, inputs]
  end

  def reduce(rules, rule=rules[0])
    case rule
    when Symbol, String
      rule
    when Integer
      reduce(rules, rules[rule])
    when Array
      rule.map { |val| reduce(rules, val) }
    end
  end

  def reduced_to_regex_str(reduced)
    return reduced if reduced.instance_of?(String)
    sym, *rest = reduced
    if sym == :cat
      rest.map { |r| reduced_to_regex_str(r) }.join('')
    elsif sym == :+
      '(' + rest.map { |r| reduced_to_regex_str(r) }.join('') + '+)'
    elsif sym == :or
      '((' + rest.map { |r| reduced_to_regex_str(r) }.join(')|(') + '))'
    else
      raise "unexpected value #{reduced} in reduced_to_regex_str"
    end
  end

  def count_matches(regex, inputs)
    inputs.select { |input| regex =~ input }.length
  end
end
