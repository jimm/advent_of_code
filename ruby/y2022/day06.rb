# Tuning Trouble

class Day06 < Day
  PART_PATTERN_LENGTH = [4, 14]

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
    chars = lines.first.split('')
    n = PART_PATTERN_LENGTH[@part_number - 1]
    (0..chars.length - n).each do |i|
      return i + n if chars[i, n].uniq.length == n
    end
  end
end
