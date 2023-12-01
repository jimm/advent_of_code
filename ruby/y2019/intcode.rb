# A simple Intcode computer. Program and memory use the same address space.
#
# ## I/O
#
# All I/O is done with integer values. Output is flushed at the start of
# every `#run`. Input is not.
#
# To send input to a computer, call `append_input`. Input is an unbuffered
# `Channel` so that we can run multiple computers in `Fiber`s.
#
# By default, output is sent to `STDOUT`. Call '#direct_output_to` to change
# a computer's output destination. Output can be sent to any `IO`, another
# `IntcodeComputer`, a `Channel(Int64)`, an `Array(Int64)`, or `Nil` (no
# output). In all cases, the most recently output `Int64` is saved into
# `last_output`.
#
# ## CPU State
#
# A computer keeps track of its ready/running/halted `CPUState`. Call
# `#state` to get the current `CPUState`, which has the predicates
# `CPUState#ready?`, `CPUState#running?`, and `CPUState#halted?`
#
# ## Memory
#
# Reads from memory are assumed to be within bounds. There is no bounds
# checking at read time.
#
# Writes to memory are assumed to be to memory locations >= 0. If the
# location is out of bounds, memory is grown so that the write succeeds.
# Attempts to access negative addresses raise an error.
#
# ## Example Code
#
# To wait for a computer to halt in a concurrent environment, keep
# `yield`ing. Here is a concurrent example that loads a program and starts
# with some initial input:
#
#     computer = IntcodeComputer.new
#     computer.load(program)
#     computer.append_input(something)
#     spawn { computer.run }
#     until computer.state.halted?
#       Fiber.yield
#     end
#
# ## Assumptions
#
# - The program will not step out of bounds
class IntcodeComputer
  attr_accessor :inputs, :running_in_fiber, :debug
  attr_reader :name, :data, :outputs

  def initialize(name: 'IC', running_in_fiber: false, debug: false)
    @pc = 0
    @data = []
    @inputs = []
    @outputs = []
    @name = name
    @running_in_fiber = running_in_fiber
    @debug = debug
  end

  # Loads a string program or data bytes into memory. If data bytes, the
  # memory is duplicated first.
  def load_memory(lines_or_data = [''])
    @pc = 0
    case lines_or_data[0]
    when String
      @data = lines_or_data[0].split(',').map(&:to_i)
    when Integer
      @data = lines_or_data.dup
    end
  end

  def clear_io
    @inputs.clear
    @outputs.clear
  end

  def run
    @pc = 0
    @running = true
    # If running in a fiber, our output might be somebody else's input
    @outputs = [] unless @running_in_fiber
    execute_next_instruction while @running
  end

  def execute_next_instruction
    byte = @data[@pc]
    opcode = extract_opcode(byte)
    modes = extract_modes(byte)

    case opcode                 # opcode
    when 1                      # add
      debug('add', modes, 4) do
        stor(load(1, modes) + load(2, modes), 3, modes)
        @pc += 4
      end
    when 2 # mult
      debug('mult', modes, 4) do
        stor(load(1, modes) * load(2, modes), 3, modes)
        @pc += 4
      end
    when 3 # input
      debug('input', modes, 2) do
        dest_addr = load(1, modes)

        # Fiber.yield while @running_in_fiber && @inputs.empty?
        Fiber.yield while @running_in_fiber && @inputs.empty?

        stor(@inputs.shift, 1, modes)
        @pc += 2
      end
    when 4 # output
      debug('output', modes, 2) do
        output = load(1, modes)
        @outputs << output
        @pc += 2
      end
    when 5                      # jump-if-true
      debug('jmp t', modes, 3) do
        if load(1, modes) != 0
          @pc = load(2, modes)
        else
          @pc += 3
        end
      end
    when 6                      # jump-if-false
      debug('jmp f', modes, 3) do
        if load(1, modes) == 0
          @pc = load(2, modes)
        else
          @pc += 3
        end
      end
    when 7                      # less than
      debug('<', modes, 4) do
        p1 = load(1, modes)
        p2 = load(2, modes)
        val = p1 < p2 ? 1 : 0
        stor(val, 3, modes)
        @pc += 4
      end
    when 8                      # equals
      debug('==', modes, 4) do
        p1 = load(1, modes)
        p2 = load(2, modes)
        val = p1 == p2 ? 1 : 0
        stor(val, 3, modes)
        @pc += 4
      end
    when 99                     # halt
      debug('halt', modes, 1) do
        @pc += 1
        @running = false
      end
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

  def debug(opcode, modes, instruction_len)
    if @debug
      bytes = @data[@pc + 1, instruction_len - 1]
      puts "#{@name}: #{opcode}\tm = #{modes}, b = #{bytes}, @inputs = #{@inputs}, @outputs = #{@outputs}"
    end
    yield
  end
end
