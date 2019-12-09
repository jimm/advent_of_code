require "../day"

module Year2019
  class Day09 < Day
    def part1
      lines = data_lines()
      if @testing
        ok = true
        lines.in_groups_of(2, "").each do |line_pair|
          ok &&= run_test1(line_pair)
        end
        puts("ok") if ok # errors already printed
      else
        boost_program = lines[0].split(",").map(&.to_i64)
        computer = IntcodeComputer.new
        computer.load(boost_program)
        computer.append_input(1)
        computer.run
      end
    end

    def run_test1(line_pair)
      control_line, program_line = line_pair
      if control_line[0] != '#'
        raise "error: malformed test file: expected '#' line saw #{control_line}"
      end

      /^#\s+(\w+)\s+(.*)/.match(control_line)
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
    end
  end
end

AoC.register(Year2019::Day09)
