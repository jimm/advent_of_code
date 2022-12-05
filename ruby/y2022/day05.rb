# Supply Stacks

class Stacker
  attr_reader :stacks

  # move_amount must be either :one or :multiple
  def run(lines, move_amount)
    @stacks = []
    @commands = []
    reading = :stacks
    lines.each do |line|
      case reading
      when :stacks
        if line.split[0, 3] == %w[1 2 3]
          reading = :commands
          @stacks.each(&:reverse!)
        else
          parse_crates(line).each_with_index do |crate, i|
            @stacks[i] ||= []
            @stacks[i].push(crate) if crate
          end
        end
      when :commands
        line =~ /move (\d+) from (\d+) to (\d+)/
        num_crates = ::Regexp.last_match(1).to_i
        from = ::Regexp.last_match(2).to_i - 1
        to = ::Regexp.last_match(3).to_i - 1
        case move_amount
        when :one
          num_crates.times { @stacks[to].push(@stacks[from].pop) }
        when :multiple
          crates = @stacks[from][-num_crates..]
          @stacks[from] = @stacks[from][0..-(num_crates + 1)]
          @stacks[to] += crates
        end
      end
    end
  end

  def parse_crates(line)
    crates = []
    while line
      crate = line[0, 4]
      line = line[4..]
      char = crate[1]
      crates << (char == ' ' ? nil : char)
    end
    crates
  end
end

class Day05 < Day
  MOVE_AMOUNT = %i[one multiple] # part 1, part 2

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
      expected = expected.split(',')[@part_number - 1]
      answer = do_part(lines)
      [answer == expected, answer, expected]
    end
  end

  def do_part(lines)
    stacker = Stacker.new
    stacker.run(lines, MOVE_AMOUNT[@part_number - 1])
    stacker.stacks.map(&:pop).join
  end
end
