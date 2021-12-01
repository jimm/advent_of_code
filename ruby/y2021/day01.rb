# Sonar Sweep

class Day01 < Day
  def part1
    nums = data_lines(1).map(&:to_i)
    puts num_increases(nums)
  end

  def part2
    nums = data_lines(1).map(&:to_i).each_cons(3).map(&:sum)
    puts num_increases(nums)
  end

  def num_increases(nums)
    num_increases = 0
    prev = nums[0]
    nums[1..-1].each do |n|
      num_increases += 1 if n > prev
      prev = n
    end
    num_increases
  end
end
