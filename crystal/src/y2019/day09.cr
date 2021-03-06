require "../day"

module Year2019
  class Day09 < Day
    def part1
      if @testing
        ok = true
        test_chunks(data_lines()).each do |data_chunk|
          result = run_test1(data_chunk[0], data_chunk[1][0])
          ok &&= result
        end
        puts("ok") if ok # errors already printed
      else
        run_with_input(1_i64)
      end
    end

    def run_test1(control_line, program_line)
      /^(\w+)\s+(.*)/.match(control_line)
      expected_type, expected_val = $1, $2
      program = program_line.split(",").map(&.to_i64)

      computer = IntcodeComputer.new
      computer.load(program)
      buf = IO::Memory.new(1024)
      computer.direct_output_to(buf)
      computer.run
      result = buf.to_s

      case expected_type
      when "nums"
        expected_output = expected_val.split(",").join("\n") + "\n"
        if result == expected_output
          true
        else
          puts("error: expected output #{expected_output} but saw #{result}")
          false
        end
      when "regex"
        regex = Regex.new(expected_val)
        if regex.match(result)
          true
        else
          puts("error: expected output to match #{regex} but saw val #{result[0]}")
        end
      else
        puts("error: mystery expected_type #{expected_type} in test file")
        false
      end
    end

    def part2
      no_tests
      run_with_input(2_i64)
    end

    def run_with_input(input)
      boost_program = data_lines(part_number: 1)[0].split(",").map(&.to_i64)
      computer = IntcodeComputer.new
      computer.load(boost_program)
      computer.append_input(input)
      computer.run
    end
  end
end

AoC.register(Year2019::Day09)
