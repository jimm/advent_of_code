# Calorie Counting

class Day01 < Day
  def part1
    lines = data_lines(1, false)
    groups = lines.slice_when { |line| line.empty? }
    sums = groups.map { |lines| lines.map(&:to_i).sum }
    puts sums.max
  end

  def part2
    lines = data_lines(1, false)
    groups = lines.slice_when { |line| line.empty? }
    sums = groups.map { |lines| lines.map(&:to_i).sum }

    max = sums.max
    top_three = max
    sums -= [max]

    max = sums.max
    top_three += max
    sums -= [max]

    max = sums.max
    top_three += max

    puts top_three
  end
end
