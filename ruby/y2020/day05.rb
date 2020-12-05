# Binary Boarding

class Day05 < Day
  def part1
    if @testing
      part1_tests
    else
      nums = data_lines(1).map { |entry| seat_num_from_code(entry) }
      puts(nums.max)
    end
  end

  def part1_tests
    errors = []
    test_chunks(1).each do |expected, lines|
      num = seat_num_from_code(lines[0])
      if num == expected.to_i
        print('.')
      else
        print('F')
        errors << "expected #{expected} got #{num}"
      end
    end
    if errors.empty?
      puts("\nok")
    else
      puts("errors: #{errors}")
    end
  end

  def part2
    no_tests()

    entries = data_lines(1)
    seats = {}
    entries.each { |entry| num = seat_num_from_code(entry); seats[num] = num }
    min, max = seats.keys.minmax
    (min+1...max).each do |seat_num|
      if seats[seat_num].nil? && !seats[seat_num - 1].nil? && !seats[seat_num + 1].nil?
        puts(seat_num)
        exit(0)
      end
    end
    puts("seat not found")
  end

  def seat_num_from_code(code)
    code
      .gsub(/[BR]/, '1')
      .gsub(/[FL]/, '0')
      .to_i(2)
  end
end
