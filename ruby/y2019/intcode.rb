class IntcodeComputer
  attr_accessor :data

  def initialize(lines)
    @data = lines[0].split(',').map(&:to_i)
    @pc = 0
  end

  def run
    @running = true
    execute_next_instruction while @running
  end

  def execute_next_instruction
    opcode = @data[@pc]
    p1 = @data[@pc + 1]
    p2 = @data[@pc + 2]
    dest_addr = @data[@pc + 3]
    case opcode                 # opcode
    when 1                      # add
      @data[dest_addr] = @data[p1] + @data[p2]
      @pc += 4
    when 2                      # mult
      @data[dest_addr] = @data[p1] * @data[p2]
      @pc += 4
    when 99                     # halt
      @pc += 1
      @running = false
    else
      raise "unknown opcode #{opcode}"
      @running = false
    end
  end
end
