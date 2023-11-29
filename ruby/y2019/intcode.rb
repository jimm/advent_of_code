class IntcodeComputer
  attr_accessor :data, :inputs

  def initialize(lines)
    @data = lines[0].split(',').map(&:to_i)
    @pc = 0
    @inputs = []
  end

  def run
    @running = true
    execute_next_instruction while @running
  end

  def execute_next_instruction
    byte = @data[@pc]
    opcode = extract_opcode(byte)
    modes = extract_modes(byte)

    case opcode                 # opcode
    when 1                      # add
      stor(load(1, modes) + load(2, modes), 3, modes)
      @pc += 4
    when 2 # mult
      stor(load(1, modes) * load(2, modes), 3, modes)
      @pc += 4
    when 3                      # input
      dest_addr = load(1, modes)
      stor(@inputs.shift, 1, modes)
      @pc += 2
    when 4                      # output
      puts load(1, modes)
      @pc += 2
    when 5                      # jump-if-true
      if load(1, modes) != 0
        @pc = load(2, modes)
      else
        @pc += 3
      end
    when 6                      # jump-if-false
      if load(1, modes) == 0
        @pc = load(2, modes)
      else
        @pc += 3
      end
    when 7                      # less than
      p1 = load(1, modes)
      p2 = load(2, modes)
      val = p1 < p2 ? 1 : 0
      stor(val, 3, modes)
      @pc += 4
    when 8                      # equals
      p1 = load(1, modes)
      p2 = load(2, modes)
      val = p1 == p2 ? 1 : 0
      stor(val, 3, modes)
      @pc += 4
    when 99                     # halt
      @pc += 1
      @running = false
    else
      raise "unknown opcode #{opcode}"
      @running = false
    end
  end

  private

  def extract_opcode(val)
    val % 100
  end

  def extract_modes(val)
    str = format('%03d', (val / 100))
    str.reverse.split('').map(&:to_i)
  end

  def load(offset, modes)
    byte = @data[@pc + offset]
    mode = modes[offset - 1]
    mode == 0 ? @data[byte] : byte
  end

  def stor(value, offset, modes)
    @data[@data[@pc + offset]] = value
  end
end
