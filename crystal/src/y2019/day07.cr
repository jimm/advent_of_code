require "../day"

module Year2019
  class Day07 < Day
    def initialize(part_number : Int32, testing : Bool)
      super
      @amplifiers = Array(IntcodeComputer).new(5) do |i|
        IntcodeComputer.new("Amp #{'A' + i}")
      end
      @amplifiers.each_cons(2) { |cons| cons[0].direct_output_to(cons[1]) }
    end

    private def _part(phase_space, run_proc)
      lines = data_lines()
      if @testing
        ok = true
        lines.in_groups_of(2, "").each do |line_pair|
          ok &&= run_test(line_pair, run_proc)
        end
        puts("ok") if ok # errors already printed
      else
        program = lines[0].split(",").map(&.to_i)
        max_signal = phase_space.each_permutation.map do |phases|
          run_proc.call(program, phases)
        end
          .max
        puts(max_signal)
      end
    end

    def part1
      run_proc = ->(program : Array(Int32), phases : Array(Int32)) do
        run_amplifiers_with_phases(program, phases)
      end
      _part([0, 1, 2, 3, 4], run_proc)
    end

    def part2
      run_proc = ->(program : Array(Int32), phases : Array(Int32)) do
        run_feedback_amplifiers_with_phases(program, phases)
      end
      _part([5, 6, 7, 8, 9], run_proc)
    end

    def run_test(line_pair, run_proc)
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
      result = run_proc.call(program, phases)
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
      result = Channel(Int32).new
      @amplifiers.last.direct_output_to(result)

      @amplifiers.zip(phases).each do |amp, phase|
        amp.load(program)
        # because done here, guaranteed to happen before receiving input
        # from previous amplifier in Fiber below
        amp.append_input(phase)
      end

      @amplifiers.each do |amp|
        spawn { amp.run }
      end

      @amplifiers.first.append_input(0)
      result.receive
    end

    def run_feedback_amplifiers_with_phases(program, phases)
      # FIXME
      raise "need to implement properly"

      result = Channel(Int32).new
      @amplifiers.last.direct_output_to(@amplifiers.first)

      @amplifiers.zip(phases).each do |amp, phase|
        amp.load(program)
        # because done here, guaranteed to happen before receiving input
        # from previous amplifier in Fiber below
        amp.append_input(phase)
      end

      @amplifiers.each do |amp|
        spawn { amp.run }
      end

      @amplifiers.first.append_input(0)
      Fiber.yield
      @amplifiers.last.last_output
    end
  end
end

AoC.register(Year2019::Day07)
