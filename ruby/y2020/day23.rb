# Crab Cups

class Node
  attr_accessor :val, :left, :right

  def initialize(val)
    @val = val
  end
end


class CrabCups
  def initialize(cups, max_cup=0)
    cups = cups.split('').map(&:to_i)
    @min_cup, @max_cup = cups.minmax
    while @max_cup < max_cup
      @max_cup += 1
      cups << @max_cup
    end

    cups = cups.map { |i| Node.new(i) }
    cups.each_with_index do |cup, i|
      cup.left = cups[i-1]
      cups[i-1].right = cup

      idx = (i+1) % cups.length
      cup.right = cups[idx]
      cups[idx].left = cup
    end

    @current_cup = cups[0]
    @nodes = {}
    cups.each { |cup| @nodes[cup.val] = cup }
  end

  def move
    # extract three cups to the right of the current cup
    three_cups_start = @current_cup.right
    three_cups_end = three_cups_start.right.right
    @current_cup.right = three_cups_end.right
    @current_cup.right.left = @current_cup

    # determine the destination cup number
    three_cups_vals = [three_cups_start.val, three_cups_start.right.val, three_cups_start.right.right.val]
    dest_cup_val = @current_cup.val - 1
    while three_cups_vals.include?(dest_cup_val)
      dest_cup_val -= 1
    end
    dest_cup_val = @max_cup if dest_cup_val < @min_cup
    while three_cups_vals.include?(dest_cup_val)
      dest_cup_val -= 1
    end

    # insert the three cups after the destination cup
    dest_cup = @nodes[dest_cup_val]
    cup_after_dest = dest_cup.right
    dest_cup.right = three_cups_start
    three_cups_start.left = dest_cup
    three_cups_end.right = cup_after_dest
    cup_after_dest.left = three_cups_end

    # update current cup
    @current_cup = @current_cup.right
  end

  def part1_answer
    cup_one = @nodes[1]
    cup = cup_one.right
    str = ''
    while cup != cup_one
      str << cup.val.to_s
      cup = cup.right
    end
    str
  end

  def part2_answer
    cup_one = @nodes[1]
    cup_one.right.val * cup_one.right.right.val
  end
end


class Day23 < Day
  TEST_INPUT = '389125467'
  REAL_INPUT = '253149867'

  def part1
    game = play(100)
    puts(game.part1_answer)
  end

  def part1_tests
    do_tests('67384529', 100)
  end

  def part2
    game = play(10_000_000, 1_000_000)
    puts(game.part2_answer)
  end

  def part2_tests
    do_tests(149245887792, 10_000_000, 1_000_000)
  end

  def do_tests(expected, num_iterations, max_cup=0)
    game = play(num_iterations, max_cup)
    answer = game.send("part#{@part_number}_answer".to_sym)
    if answer == expected
      puts('.')
      puts('ok')
    else
      puts('F')
      puts("error: expected #{expected}, got #{answer}")
    end
  end

  def play(num_iterations, max_cup=0)
    start = @testing ? TEST_INPUT : REAL_INPUT
    crab_cups = CrabCups.new(start, max_cup)
    num_iterations.times { |_| crab_cups.move }
    crab_cups
  end
end
