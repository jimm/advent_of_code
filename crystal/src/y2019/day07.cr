require "../day"

module Year2019
  class Day07 < Day
    private def _part(phase_space, run_proc)
      lines = data_lines(part_number: @testing ? @part_number : 1)
      if @testing
        ok = true
        data_chunks(lines).each do |data_chunk|
          result = run_test(data_chunk[0], data_chunk[1][0], run_proc)
          ok &&= result
        end
        puts("ok") if ok # errors already printed
      else
        program = lines[0].split(",").map(&.to_i64)
        max_signal = phase_space.each_permutation.map do |phases|
          run_proc.call(program, phases)
        end
          .max
        puts(max_signal)
      end
    end

    def part1
      run_proc = ->(program : Array(Int64), phases : Array(Int64)) do
        run_amplifiers_with_phases(program, phases)
      end
      _part([0_i64, 1_i64, 2_i64, 3_i64, 4_i64], run_proc)
    end

    def part2
      run_proc = ->(program : Array(Int64), phases : Array(Int64)) do
        run_feedback_amplifiers_with_phases(program, phases)
      end
      _part([5_i64, 6_i64, 7_i64, 8_i64, 9_i64], run_proc)
    end

    def run_test(control_line, program_line, run_proc)
      control_regex = /signal (\d+).*sequence ([\d,]+)/
      control_regex.match(control_line)
      expected_signal, phases_str = $1.to_i, $2
      phases = phases_str.split(",").map(&.to_i64)
      raise "error: expected 5 phases" if phases.size != 5

      program = program_line.split(",").map(&.to_i64)
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
      amplifiers = build_amplifiers(program)
      result = Channel(Int64).new
      amplifiers.last.direct_output_to(result)

      init_and_start_amplifiers(amplifiers, phases)
      amplifiers.first.append_input(0)
      result.receive
    end

    def run_feedback_amplifiers_with_phases(program, phases)
      amplifiers = build_amplifiers(program)
      result = Channel(Int64).new
      amplifiers.last.direct_output_to(amplifiers.first)

      init_and_start_amplifiers(amplifiers, phases)
      amp_e = amplifiers.last
      amplifiers.first.append_input(0)
      until amp_e.state.halted?
        Fiber.yield
      end
      amp_e.last_output
    end

    def build_amplifiers(program)
      amplifiers = Array(IntcodeComputer).new(5) do |i|
        IntcodeComputer.new("Amp #{'A' + i}")
      end
      amplifiers.each_cons(2) { |cons| cons[0].direct_output_to(cons[1]) }
      amplifiers.each { |amp| amp.load(program) }
      amplifiers
    end

    def init_and_start_amplifiers(amplifiers, phases)
      amplifiers.zip(phases).each do |amp, phase|
        # because done here, guaranteed to happen before receiving input
        # from previous amplifier in Fiber below
        amp.append_input(phase)
      end

      amplifiers.each do |amp|
        spawn { amp.run }
      end
    end
  end
end

AoC.register(Year2019::Day07)
