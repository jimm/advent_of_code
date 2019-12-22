enum ParamMode
  Indirect # also called "position mode"
  Immediate
  Relative
  Unknown # when dumping data
end

enum CPUState
  Ready
  Running
  Halted
end

# A simple 64-bit Intcode computer. Program and memory use the same address
# space.
#
# ## I/O
#
# All I/O is done with `Int64` values. Output is flushed at the start of
# every `#run`. Input is not.
#
# To send input to a computer, call `append_input`. Input is buffered with a
# buffer size of 1024. If the buffer is full, the caller **will be
# blocked**. Input uses `Channel`s so that we can run multiple computers in
# `Fiber`s.
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
  @@BUFSIZ = 1024

  getter name : String
  getter state : CPUState
  getter last_output : Int64
  @output_io : IO | IntcodeComputer | Channel(Int64) | Array(Int64) | Nil

  def initialize(@name = "Computer")
    @mem = [] of Int64
    @pc = 0_i64
    @opcode = 0_i64
    @param_modes = [] of ParamMode
    @input_channel = Channel(Int64).new(@@BUFSIZ)
    @output_io = STDOUT
    @trace = false
    @last_output = 0_i64
    @state = CPUState::Ready
    @relative_base = 0_i64
  end

  # ================ Program loading and execution ================

  # Stores a copy of *program* in memory, completely replacing what was
  # there.
  def load(program)
    @mem = program.dup
  end

  # Runs the program starting at address 0. Stops when a `halt` instruction
  # is run.
  def run
    @state = CPUState::Running
    # Initialize output. Do not initialize input.
    flush_output()
    @pc = 0_i64
    @relative_base = 0_i64

    while true
      parse_opcode_at_pc
      case @opcode
      when 1 # add
        do_trace("add", 3, 3) do
          dest = addr_at_param(3)
          p1 = val_of_param(1)
          p2 = val_of_param(2)
          set(dest, p1 + p2)
        end
        @pc += 4
      when 2 # mult
        do_trace("mult", 3, 3) do
          dest = addr_at_param(3)
          p1 = val_of_param(1)
          p2 = val_of_param(2)
          set(dest, p1 * p2)
        end
        @pc += 4
      when 3 # input
        do_trace("in", 1, 1) do
          dest = addr_at_param(1)
          val = get_input()
          set(dest, val)
        end
        @pc += 2
      when 4 # output
        do_trace("out", 1, 1) do
          val = val_of_param(1)
          append_output(val)
          val
        end
        @pc += 2
      when 5 # jump if non-zero
        trace_instruction("jnz", 2)
        p1 = val_of_param(1)
        p2 = val_of_param(2)
        if p1 != 0
          @pc = p2
        else
          @pc += 3
        end
      when 6 # jump if zero
        trace_instruction("jz", 2)
        p1 = val_of_param(1)
        p2 = val_of_param(2)
        if p1 == 0
          @pc = p2
        else
          @pc += 3
        end
      when 7 # lt
        do_trace("lt", 3) do
          dest = addr_at_param(3)
          p1 = val_of_param(1)
          p2 = val_of_param(2)
          set(dest, p1 < p2 ? 1_i64 : 0_i64)
        end
        @pc += 4
      when 8 # eq
        do_trace("eq", 3) do
          dest = addr_at_param(3)
          p1 = val_of_param(1)
          p2 = val_of_param(2)
          set(dest, p1 == p2 ? 1_i64 : 0_i64)
        end
        @pc += 4
      when 9 # adjust relative base
        do_trace("rbadj", 1) do
          @relative_base += val_of_param(1)
        end
        @pc += 2
      when 99 # halt
        trace_instruction("halt", 0)
        @state = CPUState::Halted
        return
      else
        raise("@pc #{@pc} error: unknown opcode #{@opcode}")
        return
      end
    end
  end

  # This is by no means perfect. It doesn't handle unknown instructions
  # (locations used as memory) well at all.
  def dump_memory(assume_first_zero_is_data_region = false)
    old_pc = @pc
    @pc = 0
    old_trace = @trace
    @trace = true

    while @pc < @mem.size
      if @mem[@pc] >= 0
        parse_opcode_at_pc
      else
        @opcode = 98 # constant
      end
      case @opcode
      when 1 # add
        trace_instruction("add", 3, 3)
        @pc += 4
      when 2 # mult
        trace_instruction("mult", 3, 3)
        @pc += 4
      when 3 # input
        trace_instruction("in", 1, 1)
        @pc += 2
      when 4 # output
        trace_instruction("out", 1, 1)
        @pc += 2
      when 5 # jump if non-zero
        trace_instruction("jnz", 2)
        @pc += 3
      when 6 # jump if zero
        trace_instruction("jz", 2)
        @pc += 3
      when 7 # lt
        trace_instruction("lt", 3)
        @pc += 4
      when 8 # eq
        trace_instruction("eq", 3)
        @pc += 4
      when 9 # adjust relative base
        trace_instruction("rbadj", 1)
        @pc += 2
      when 99 # halt
        trace_instruction("halt", 0)
        @pc += 1
      else
        if @opcode == 0 && assume_first_zero_is_data_region
          while @pc < @mem.size
            puts("#{"%08d" % @pc}: #{@mem[@pc]}")
            @pc += 1
          end
        else
          puts("#{"%08d" % @pc}: #{@mem[@pc]}")
          @pc += 1
        end
      end
    end
    @trace = old_trace
    @pc = old_pc
  end

  # ================ Memory I/O ================

  # Returns the value of memory location *loc*. Uninitialized memory
  # locations return `0_i64`.
  #
  # If necessary, the size of memory is increased so that *loc* is within
  # bounds.
  def get(loc)
    address_check(loc)
    @mem[loc]
  end

  # Sets the value of memory location *loc* to *val*.
  #
  # If necessary, the size of memory is increased so that *loc* is within
  # bounds.
  def set(loc, val : Int64)
    address_check(loc)
    @mem[loc] = val
  end

  def address_check(loc)
    raise "error: attempt to access loc #{loc}" if loc < 0
    if loc >= @mem.size
      @mem.concat(Array(Int64).new(loc - @mem.size + 1, 0))
    end
  end

  # ================ I/O ================

  # ---------------- Input ----------------

  # Sends *num* to this computer's input chanel.
  #
  # Input is buffered with a buffer size of 1024. If the buffer is full, the
  # caller **will be blocked**. Input uses `Channel`s so that we can run
  # multiple computers in `Fiber`s.
  def append_input(num : Int64)
    puts("#{name}#append_input #{num}") if @trace
    @input_channel.send(num)
    puts("#{name}#append_input back from sending") if @trace
  end

  # Receives input from our input `Channel`.
  def get_input
    puts("#{name}#get_input") if @trace
    val = @input_channel.receive.tap do |val|
      puts("#{name}#get_input received input #{val}") if @trace
    end
    val
  end

  # ---------------- Output ----------------

  # Directs all output to *stream*.
  def direct_output_to(stream)
    @output_io = stream
  end

  # Appends a single *val* to the current output stream.
  def append_output(val)
    @last_output = val
    case @output_io
    when IntcodeComputer
      @output_io.as(IntcodeComputer).append_input(val)
    when IO
      @output_io.as(IO).puts(val)
    when Channel(Int64)
      @output_io.as(Channel(Int64)).send(val)
    when Array(Int64)
      @output_io.as(Array(Int64)) << val
    end
  end

  # Empties the output queue. Output is flushed at the start of every
  # `#run`. (Input is not.)
  def flush_output
    @output_queue = [] of Int64
  end

  # ================ Debugging ================

  def trace(val : Bool)
    @trace = val
  end

  def trace_instruction(name, num_params, must_be_addr = -1, newline = true)
    return unless @trace
    param_strs = (0...num_params).map do |i|
      offset = i + 1
      mode_char = i == must_be_addr ? '@' : mode_char(offset)
      if @pc + offset >= @mem.size
        "???"
      else
        "#{mode_char}#{get(@pc + offset)}"
      end
    end
    print("#{"%08d" % @pc}: #{name}\t#{param_strs.join(", ")}")
    puts() if newline
  end

  def trace_result(val)
    puts("\t\t=> #{val}") if @trace
  end

  def do_trace(name, num_params, must_be_addr = -1)
    trace_instruction(name, num_params, must_be_addr, newline = false)
    val = yield
    trace_result(val)
  end

  # ================ CPU ================

  # Reads the instruction at `@pc` and sets `@opcode` and `@param_modes`.
  def parse_opcode_at_pc
    param_mode_num, @opcode = get(@pc).divmod(100)
    @param_modes = param_mode_num.to_s.reverse.chars.map do |d|
      n = d.to_i
      n = 3 if n > 3
      ParamMode.new(n)
    end
  end

  # Returns the `ParamMode` of parameter *offset*.
  def mode_of_param(offset)
    (offset - 1) < @param_modes.size ? @param_modes[offset - 1] : ParamMode::Indirect
  end

  # Returns a character denoting the `ParamMode` of parameter *offset*
  # (starting at 1).
  def mode_char(offset)
    ['@', '#', 'r', '?'][mode_of_param(offset).to_i]
  end

  # Returns the value of parameter *offset*, taking into account the param
  # mode for that parameter. Assumes the program counter is pointing to the
  # beginning of the current instruction.
  def val_of_param(offset) : Int64
    param = get(@pc + offset)
    param_mode_digit = mode_of_param(offset)
    case param_mode_digit
    when ParamMode::Indirect
      get(param)
    when ParamMode::Immediate
      param
    when ParamMode::Relative
      get(param + @relative_base)
    else
      raise "error: illegal param mode digit #{param_mode_digit}"
    end
  end

  # Returns the destination address of parameter *offset*, taking into
  # account the param mode for that parameter. `ParamMode::Immediate` is
  # illegal. Assumes the program counter is pointing to the beginning of the
  # current instruction.
  def addr_at_param(offset) : Int64
    param = get(@pc + offset)
    param_mode_digit = mode_of_param(offset)
    case param_mode_digit
    when ParamMode::Indirect
      param
    when ParamMode::Immediate
      raise("error: immediate address mode is illegal")
    when ParamMode::Relative
      param + @relative_base
    else
      raise "error: illegal param mode digit #{param_mode_digit}"
    end
  end
end
