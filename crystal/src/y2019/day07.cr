require "../day"

module Year2019
  class Day07 < Day
    def initialize(part_number : Int32, testing : Bool)
      super
      @amplifiers = Array(IntcodeComputer).new(5) { |_| IntcodeComputer.new }
      @amplifiers.each_cons(2) { |cons| cons[0].direct_output_to(cons[1]) }
    end

    def part1
      lines = data_lines()
      if @testing
        ok = true
        lines.in_groups_of(2, "").each do |line_pair|
          ok &&= run_test1(line_pair)
        end
        puts("ok") if ok # errors already printed
      else
        phase_space = [0, 1, 2, 3, 4]
        program = lines[0].split(",").map(&.to_i)
        max_signal = phase_space.each_permutation.map do |phases|
          run_amplifiers_with_phases(program, phases)
        end
          .max
        puts(max_signal)
      end
    end

    def part2
      lines = data_lines()
      if @testing
        ok = true
        lines.in_groups_of(2, "").each do |line_pair|
          ok &&= run_test1(line_pair)
        end
        puts("ok") if ok # errors already printed
      else
        phase_space = [5, 6, 7, 8, 9]
        program = lines[0].split(",").map(&.to_i)
        # FIXME
        max_signal = phase_space.each_permutation.map do |phases|
          run_amplifiers_with_phases(program, phases)
        end
          .max
        puts(max_signal)
      end
    end

    def run_test1(line_pair)
      control_line, program_line = line_pair
      if control_line[0] != '#'
        raise "error: malformed test file: expected '#' line saw #{control_line}"
      end

      control_regex = /signal (\d+).*sequence ([\d,]+)/
      control_regex.match(control_line)
      expected_signal, phases_str = $1.to_i, $2
      phases = phases_str.split(",").map(&.to_i)
      raise "error: expected 5 phases" if phases.size != 5

      program = program_line.split(",").map(&.to_i)
      result = run_amplifiers_with_phases(program, phases)
      if result == expected_signal
        true
      else
        puts("error: expected final signal #{expected_signal} but saw #{result}")
        false
      end
    end

    # Runs five amplifiers in series after inputting the given `phases`.
    # First amp starts with input zero, each one feeds its output to the
    # next input. Returns final amplifier output.
    def run_amplifiers_with_phases(program, phases)
      last_amp_output = [] of Int32
      @amplifiers.last.direct_output_to(last_amp_output)
      @amplifiers.zip(phases).each do |amp, phase|
        amp.load(program)
        amp.append_input(phase)
      end

      @amplifiers.first.append_input(0)
      @amplifiers.each(&.run)
      last_amp_output[-1]
    end

    def program_amplifiers(program)
      @amplifiers.each { |amp| amp.load(program) }
    end
  end
end

AoC.register(Year2019::Day07)
