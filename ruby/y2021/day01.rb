# Puzzle Name

class Day01 < Day
  def part1
    nums = data_lines(1).map(&:to_i)
    num_increases = 0
    prev = nums[0]
    nums[1..-1].each do |n|
      num_increases += 1 if n > prev
      prev = n
    end
    puts num_increases
  end

  def part2
    nums = data_lines(1).map(&:to_i)
    num_increases = 0
    prev = 9999
    nums.each_cons(3) do |arr|
      val = arr.sum
      num_increases += 1 if val > prev
      prev = val
    end
    puts num_increases
  end
end
