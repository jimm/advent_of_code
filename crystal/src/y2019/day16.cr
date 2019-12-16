require "../day"

module Year2019
  class Day16 < Day
    def part1
      if @testing
        ok = true
        data_lines().in_groups_of(2, "").each do |lines|
          expected = lines[0][2..]
          input = lines[1].split("").map(&.to_i)
          result = test1(expected, input)
          ok &&= result
        end
        puts("ok") if ok
      else
        input = data_lines()[0].split("").map(&.to_i)
        output = fft(input)
        puts(output.join("")[0, 8])
      end
    end

    def part2
      if @testing
        ok = true
        data_lines().in_groups_of(2, "").each do |lines|
          expected = lines[0][2..]
          input = lines[1].split("").map(&.to_i).cycle(10_000).to_a
          result = test2(expected, input)
          ok &&= result
        end
        puts("ok") if ok
      else
        input = data_lines()[0].split("").map(&.to_i).cycle(10_000).to_a
        output = fft(input)
        offset = output[0, 7].join("").to_i64
        final_message = input[offset, 8].join("")
        puts(final_message)
      end
    end

    def fft(input)
      len = input.size
      output = input
      100.times do |i|
        new_output = (0...len).map do |j|
          groups = output[j..].in_groups_of(j + 1, 0) # 1's, 0's, -1's, 0's
          total = 0
          groups.in_groups_of(4, [0]).each do |group|
            ones, _zeroes_1, neg_ones, _zeroes_2 = group
            total += ones.sum
            total -= neg_ones.sum
          end
          total.abs % 10
        end
        output = new_output
      end
      output
    end

    def test1(expected, input)
      output = fft(input)
      output_str = output[0, 8].join("")
      if output_str != expected
        puts("output #{output_str} != expected #{expected}")
        return false
      end
      true
    end

    def test2(expected, input)
      output = fft(input)
      offset = output[0, 7].join("").to_i64
      final_message = input[offset, 8].join("")
      if final_message != expected
        puts("final message #{final_message} != expected #{expected}")
        return false
      end
      true
    end
  end
end

AoC.register(Year2019::Day16)
