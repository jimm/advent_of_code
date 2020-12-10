# Adapter Array

class Day10 < Day
  def part1
    puts(do_part1(data_lines()))
  end

  def part1_tests
    run_tests(:do_part1)
  end

  def do_part1(lines)
    adapters = adapters_from_lines(lines)
    diffs = {1 => 0, 3 => 0}
    (0..adapters.length-2).each do |i|
      j0 = adapters[i]
      j1 = adapters[i+1]
      diff = j1 - j0
      if diff != 1 && diff != 3
        raise "unexpected diff #{diff} between #{j0} and #{j1}"
      end
      diffs[diff] += 1
    end
    diffs[1] * diffs[3]
  end

  def part2
    puts(do_part2(data_lines()))
  end

  def part2_tests
    run_tests(:do_part2)
  end

  def do_part2(lines)
    adapters = adapters_from_lines(lines)
    count_paths(adapters, {})
  end

  def can_be_next?(val, next_val)
    !next_val.nil? && (next_val - val) <= 3
  end

  def count_paths(adapters, acc)
    return 1 if adapters.length < 2
    return acc[adapters] if acc.has_key?(adapters)

    val = adapters[0]
    paths = []
    paths << adapters[1..-1] if can_be_next?(val, adapters[1])
    paths << adapters[2..-1] if can_be_next?(val, adapters[2])
    paths << adapters[3..-1] if can_be_next?(val, adapters[3])
    answer = paths.map { |path| count_paths(path, acc) }.sum
    acc[adapters] = answer
    answer
  end

  def adapters_from_lines(lines)
    adapters = lines.map(&:to_i).sort
    # Put 0 (wall plug) at the front and device (max + 3) at the end.
    adapters.unshift(0)
    adapters << adapters.max + 3
    adapters
  end

  def run_tests(func_sym)
    errors = []
    test_chunks(1).each do |chunk_comment, lines|
      expected = expected_for_part(chunk_comment)
      answer = send(func_sym, lines)
      if expected == answer
        print('.')
      else
        print('F')
        errors << "expected #{expected} != answer #{answer}"
      end
    end
    puts()
    if errors.empty?
      puts("ok")
    else
      puts(errors)
    end
  end

  def expected_for_part(chunk_comment)
    eval(chunk_comment)[@part_number - 1]
  end
end
