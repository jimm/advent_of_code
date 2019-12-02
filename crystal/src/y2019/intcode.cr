# A simple Intcode computer. Program and memory use the same address space.
#
# Assumptions:
# - The program will not step out of bounds
class IntcodeComputer
  def initialize
    @mem = [] of Int32
  end

  def load(program)
    @mem = program.dup
  end

  def run
    pc = 0
    while true
      case @mem[pc]
      when 1 # add
        @mem[@mem[pc + 3]] = @mem[@mem[pc + 1]] + @mem[@mem[pc + 2]]
        pc += 4
      when 2 # mult
        @mem[@mem[pc + 3]] = @mem[@mem[pc + 1]] * @mem[@mem[pc + 2]]
        pc += 4
      when 99 # halt
        return
      else
        puts("error: unknown opcode #{@mem[pc]}")
        exit(0)
      end
    end
  end

  def get(loc)
    @mem[loc]
  end

  def set(loc, val)
    @mem[loc] = val
  end
end
