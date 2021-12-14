# Extended Polymerization

class Day14 < Day
  def part1
    do_part(10)
  end

  def part2
    do_part(40)
  end

  def do_part(steps)
    lines = data_lines(1)
    polymer_template = lines.shift
    rules = {}
    lines.each do |line|
      line =~ /(..) -> (.)/
      rules[Regexp.last_match(1)] = Regexp.last_match(2)
    end
    steps.times { |_| polymer_template = apply_rules(polymer_template, rules) }
    puts score(polymer_template)
  end

  def apply_rules(str, rules)
    new_str = ''
    (0..str.length - 2).each do |i|
      two_chars = str[i, 2]
      middle = rules[two_chars]
      new_str << str[i] + middle
    end
    new_str << str[-1]
    new_str
  end

  def score(str)
    groups = str.split('').group_by { |ch| ch }
    lens = groups.values.map(&:length)
    min_len = lens.min
    max_len = lens.max
    max_len - min_len
  end
end
