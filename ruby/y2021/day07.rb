# The Treachery of Whales

class Day07 < Day
  def part1
    positions = data_lines(1).first.split(',').map(&:to_i)
    min_used = min_fuel(positions) do |target|
      positions.reduce(0) do |acc, pos|
        acc + (target - pos).abs
      end
    end
    puts min_used
  end

  def part2
    positions = data_lines(1).first.split(',').map(&:to_i)
    min_used = min_fuel(positions) do |target|
      positions.reduce(0) do |acc, pos|
        n = (target - pos).abs
        acc + (n * n + n) / 2
      end
    end
    puts min_used
  end

  def min_fuel(positions)
    min = positions.min
    max = positions.max
    used = []
    min.upto(max) do |pos|
      used << yield(pos)
    end
    used.min
  end
end
