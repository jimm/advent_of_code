# Crab Cups

class CrabCups
  def initialize(cups)
    @cups = cups.split('').map(&:to_i)
    @num_cups = @cups.length
    @min_cup, @max_cup = @cups.minmax
  end

  # Instead of constantly moving the current cup index, we keep that at 0 so
  # that shuffling things around is much, much easier.
  def move
    three_cups = @cups[1, 3]

    dest_cup = @cups[0] - 1
    while three_cups.include?(dest_cup)
      dest_cup -= 1
    end
    dest_cup = @max_cup if dest_cup < @min_cup
    while three_cups.include?(dest_cup)
      dest_cup -= 1
    end
    dest_cup_index = @cups.index(dest_cup)

    first = @cups.first
    @cups = @cups[4..dest_cup_index] + three_cups + @cups[dest_cup_index+1..-1]
    @cups << first
  end

  def part1_answer
    if @cups[0] == 1
      @cups[1..-1].join
    elsif @cups[-1] == 1
      @cups[0..-2].join
    else
      one_index = @cups.index(1)
      (@cups[one_index+1..-1] + @cups[0..one_index-1]).join('')
    end
  end
end


class BiggerCrabCups < CrabCups
  def initialize(cups)
    super
    while @max_cup <= 1_000_000
      @max_cup += 1
      @cups << @max_cup
    end
    @cache = {}
  end

  def move
    cached = @cache[@cups]
    return cached if cached
    curr_cups = @cups.dup
    super
    @cache[curr_cups] = @cups.dup
  end

  def part2_answer
    one_index = @cups.index(1)
    @cups[one_index + 1] * @cups[one_index + 2]
  end
end

class Day23 < Day
  def part1
    puts(do_part1('253149867'))
  end

  def part1_tests
    expected = '67384529'
    answer = do_part1('389125467')
    if answer == expected
      puts('.')
      puts('ok')
    else
      puts('F')
      puts("error: expected #{expected}, got #{answer}")
    end
  end

  def do_part1(start)
    crab_cups = CrabCups.new(start)
    100.times { |_| crab_cups.move }
    crab_cups.part1_answer
  end

  def part2
  end

  def part2_tests
    expected = 149245887792
    answer = do_part2('389125467')
    if answer == expected
      puts('.')
      puts('ok')
    else
      puts('F')
      puts("error: expected #{expected}, got #{answer}")
    end
  end

  def do_part2(start)
    crab_cups = BiggerCrabCups.new(start)
    10_000_000.times { |_| crab_cups.move }
    crab_cups.part2_answer
  end
end
