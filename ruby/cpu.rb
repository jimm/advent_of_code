class Instruction
  attr_accessor :op, :val

  def initialize(op, val)
    @op = op
    @val = val
  end
end

class CPU
  attr_reader :acc, :instructions, :pc

  def initialize
    @acc = 0
    @pc = 0
    @instructions = []
  end

  def run
    @acc = 0
    @pc = 0
    execute_instruction(@instructions[@pc]) while pc_legal?
  end

  def pc_legal?
    (0...@instructions.length).include?(@pc)
  end

  def execute_instruction(instruction)
    case instruction.op
    when :acc
      @acc += instruction.val
      @pc += 1
    when :jmp
      @pc += instruction.val
    when :nop
      @pc += 1
    else
      raise "illegal instruction #{instruction.inspect}"
    end
  end

  def halt
    @pc = @instructions.length
  end

  def load_program(lines)
    @instructions = lines.map do |line|
      op, val = line.split
      Instruction.new(op.to_sym, val.to_i)
    end
  end
end
