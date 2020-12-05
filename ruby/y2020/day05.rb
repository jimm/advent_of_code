# Binary Boarding

class Day05 < Day
  def part1
    nums = data_lines(1).map { |entry| seat_num_from_code(entry) }
    if @testing
      errors = []
      answers = [357, 567, 119, 820]
      nums.zip(answers).each do |num, answer|
        if num == answer
          print('.')
        else
          print('F')
          errors << "expected #{answer} got #{num}"
        end
      end
      if errors.empty?
        puts("\nok")
      else
        puts("errors: #{errors}")
      end
    else
      print(nums.max)
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
    binary_text = code.gsub('F', '0').gsub('B', '1').gsub('R', '1').gsub('L', '0')
    binary_text.to_i(2)
  end
end
