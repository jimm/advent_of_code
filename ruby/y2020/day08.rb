# Handheld Halting

require_relative '../cpu'

class LoopDetective < CPU
  attr_reader :infinite_loop_detected
  alias :infinite_loop_detected? :infinite_loop_detected

  def initialize
    super
  end

  def run
    @pcs_execution_counts = []
    @infinite_loop_detected = false
    super
  end

  def execute_instruction(instruction)
    @pcs_execution_counts[@pc] = (@pcs_execution_counts[@pc] || 0) + 1
    if @pcs_execution_counts[@pc] > 1
      @infinite_loop_detected = true
      halt
    else
      super
    end
  end
end

class Day08 < Day
  def part1
    cpu = LoopDetective.new
    cpu.load_program(data_lines(1))
    cpu.run
    puts(cpu.acc)
  end

  def part2
    cpu = LoopDetective.new
    cpu.load_program(data_lines(1))

    modified_pc = -1
    while true
      cpu.run
      if cpu.infinite_loop_detected?
        flip_instruction_at(cpu, modified_pc) if modified_pc >= 0
        modified_pc = next_instruction_to_modify(cpu, modified_pc)
        flip_instruction_at(cpu, modified_pc)
      else
        break
      end
    end
    puts(cpu.acc)
  end

  def flip_instruction_at(cpu, pc)
    case cpu.instructions[pc].op
    when :nop
      cpu.instructions[pc].op = :jmp
    when :jmp
      cpu.instructions[pc].op = :nop
    else
      raise "illegal flip at pc #{pc}"
    end
  end

  def next_instruction_to_modify(cpu, modified_pc)
    modified_pc += 1
    while ![:nop, :jmp].include?(cpu.instructions[modified_pc].op)
      modified_pc += 1
    end
    modified_pc
  end
end
