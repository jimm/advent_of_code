# Monkey in the Middle

class Monkey
  attr_reader :items, :items, :operation, :test_mod, :true_monkey,
              :false_monkey, :num_inspected
  attr_accessor :worry_divisor, :worry_modulo

  def initialize(num)
    @num = num
    @worry_divisor = @worry_modulo = nil
    @items = []
    @num_inspected = 0
  end

  def round
    retval = []
    results = @items.map do |item|
      worry_level = eval_operation(item)
      worry_level /= @worry_divisor if @worry_divisor
      worry_level = (worry_level % @worry_modulo) if @worry_modulo
      [throw_to_monkey(worry_level), worry_level]
    end
    @num_inspected += @items.length
    @items = []
    results
  end

  def catch_item(val)
    @items.push(val)
  end

  def parse(line)
    case line
    when /Starting items: (.*)/
      @items = ::Regexp.last_match(1).split(', ').map(&:to_i)
    when
      /Operation: new = old (.) (.*)/
      @operation = ::Regexp.last_match(1).to_sym
      @amount = ::Regexp.last_match(2)
      @amount = @amount.to_i if @amount != 'old'
    when
      /divisible by (\d+)/
      @test_mod = ::Regexp.last_match(1).to_i
    when /If true: throw to monkey (\d+)/
      @true_monkey = ::Regexp.last_match(1).to_i
    when /If false: throw to monkey (\d+)/
      @false_monkey = ::Regexp.last_match(1).to_i
    end
  end

  def eval_operation(old)
    old.send(@operation, @amount == 'old' ? old : @amount)
  end

  def throw_to_monkey(worry_level)
    (worry_level % @test_mod) == 0 ? @true_monkey : @false_monkey
  end

  def to_s
    "Monkey #{@num}: #{@items.inspect}, num_inspected = #{num_inspected}"
  end
end

class Day11 < Day
  NUM_ROUNDS = [20, 10_000]

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
    monkeys = parse(lines)

    NUM_ROUNDS[@part_number - 1].times do |round|
      monkeys.each do |m|
        targets_and_items = m.round
        targets_and_items.each do |target, item|
          monkeys[target].catch_item(item)
        end
      end
    end
    monkeys
      .sort_by { |monkey| monkey.num_inspected }
      .reverse[0, 2]
      .map(&:num_inspected)
      .reduce(&:*)
  end

  def parse(lines)
    monkeys = []
    monkey = nil
    monkey_index = 0
    lines.each do |line|
      if line =~ /Monkey/
        monkey = Monkey.new(monkey_index)
        monkeys << monkey
        monkey_index += 1
      else
        monkey.parse(line)
      end
    end
    if @part_number == 1
      monkeys.each { _1.worry_divisor = 3 }
    else
      modulo = monkeys.map(&:test_mod).reduce(&:*)
      monkeys.each { _1.worry_modulo = modulo }
    end
    monkeys
  end
end
