# Encoding Error

class Day09 < Day
  def part1
    data = data_lines(1).map(&:to_i)
    puts(invalid_number(data, 25))
  end

  def part1_tests
    data = data_lines(1).map(&:to_i)
    puts(invalid_number(data, 5))
  end

  def invalid_number(input, window_size)
    window, data = input[0..window_size-1], input[window_size..-1]
    while !data.empty?
      return data[0] if !sum_of_two_previous_different(window, data[0])
      window.shift(1)
      window.push(data[0])
      data.shift(1)
    end
    raise "invalid number not found"
  end

  def sum_of_two_previous_different(window, val)
    sums = window.combination(2).map(&:sum)
    sums.include?(val)
  end

  def part2
    data = data_lines(1).map(&:to_i)
    puts(do_part2(data, 25))
  end

  def part2_tests
    data = data_lines(1).map(&:to_i)
    puts(do_part2(data, 5))
  end

  def do_part2(data, window_size)
    n = invalid_number(data, window_size)
    while !data.empty?
      sum_end_index = find_sum_end_index(n, data)
      if sum_end_index
        min, max = data[0..sum_end_index].minmax
        return min + max
      end
      data.shift(1)
    end
  end

  def find_sum_end_index(n, data)
    sum = 0
    i = 0
    while sum < n
      sum += data[i]
      return i if sum == n
      i += 1
    end
    nil
  end
end
