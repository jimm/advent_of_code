#!/usr/bin/env ruby
#
# Amplification Circuit

require_relative '../day'
require_relative 'intcode'

class Day07 < Day
  def do_part1(lines)
    program = lines[0].split(',').map(&:to_i)
    find_max_output_phase_setting(program)
  end

  def do_part2(lines)
    program = lines[0].split(',').map(&:to_i)
    find_max_output_phase_setting_with_feedback(program)
  end

  private

  def find_max_output_phase_setting(program)
    amp = IntcodeComputer.new
    max = 0
    (0..4).to_a.permutation.each do |phases|
      input = 0
      result = 0
      phases.each do |phase|
        amp.load_memory(program)
        amp.inputs = [phase, input]
        amp.run
        output = amp.outputs[0]
        input = output
      end
      max = input if input > max
    end
    max
  end

  def find_max_output_phase_setting_with_feedback(program)
    prev_amp = nil
    amps = (0..4).map do |i|
      IntcodeComputer.new(name: "Amp #{('A'.ord + i).chr}", running_in_fiber: true).tap do |amp|
        amp.inputs = prev_amp.outputs if prev_amp
        prev_amp = amp
      end
    end
    amps[0].inputs = amps[-1].outputs

    amps.each do |amp|
      warn "#{amp.name}: amp.object_id = #{amp.object_id}, amp.inputs.object_id = #{amp.inputs.object_id}, amp.outputs.object_id = #{amp.outputs.object_id}"
    end

    max = 0
    (5..9).to_a.permutation.each do |phases|
      amps.each(&:clear_io)
      amps.each_with_index do |amp, i|
        amp.load_memory(program)
        amp.inputs << phases[i]
      end
      amps[0].inputs << 0
      fibers = amps.map do |amp|
        warn "about to run #{amp.name}: amp.inputs = #{amp.inputs}, amp.outputs = #{amp.outputs}"
        Fiber.new { amp.run }
      end
      fibers.each(&:resume)
      output = amps[-1].outputs[-1]
      max = output if output > max
    end
    max
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
