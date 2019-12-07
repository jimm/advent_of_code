enum ParamMode
  Indirect
  Immediate
end

# A simple Intcode computer. Program and memory use the same address space.
#
# Assumptions:
# - The program will not step out of bounds
class IntcodeComputer
  @output_io : IO | IntcodeComputer | Array(Int32) | Nil

  def initialize
    @mem = [] of Int32
    @pc = 0
    @opcode = 0
    @param_modes = [] of ParamMode
    @input_queue = [] of Int32
    @output_io = STDOUT
    @trace = false
  end

  # ================ Program loading and execution ================

  # Stores a copy of `program` in memory.
  def load(program)
    @mem = program.dup
  end

  # Runs the program starting at address 0. Stops when halt is seen.
  def run
    # Initialize output. Do not initialize input.
    flush_output()
    @pc = 0

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
        do_trace("input", 1, 1) do
          dest = addr_at_param(1)
          val = get_input()
          set(dest, val)
        end
        @pc += 2
      when 4 # output
        do_trace("output", 1, 1) do
          val = val_of_param(1)
          append_output(val)
        end
        @pc += 2
      when 5 # jump if non-zero
        p1 = val_of_param(1)
        p2 = val_of_param(2)
        trace_instruction("jnz", 2)
        if p1 != 0
          @pc = p2
        else
          @pc += 3
        end
      when 6 # jump if zero
        p1 = val_of_param(1)
        p2 = val_of_param(2)
        trace_instruction("jz", 2)
        if p1 == 0
          @pc = p2
        else
          @pc += 3
        end
      when 7
        do_trace("lt", 3) do
          dest = addr_at_param(3)
          p1 = val_of_param(1)
          p2 = val_of_param(2)
          set(dest, p1 < p2 ? 1 : 0)
        end
        @pc += 4
      when 8
        do_trace("eq", 3) do
          dest = addr_at_param(3)
          p1 = val_of_param(1)
          p2 = val_of_param(2)
          set(dest, p1 == p2 ? 1 : 0)
        end
        @pc += 4
      when 99 # halt
        trace_instruction("halt", 0)
        return
      else
        puts("@pc #{@pc} error: unknown opcode #{@opcode}")
        return
      end
    end
  end

  # ================ Memory I/O ================

  def get(loc)
    if loc < 0 || loc >= @mem.size
      puts("@pc #{@pc} error: memory location #{loc} is out of bounds, get returning 0")
      return 0
    end
    @mem[loc]
  end

  def set(loc, val : Int32)
    if loc < 0
      puts("@pc #{@pc} error: memory location #{loc} is out of bounds, ignoring")
      return
    end
    if loc >= @mem.size
      @mem.concat(Array(Int32).new(loc - @mem.size + 1))
    end
    @mem[loc] = val
  end

  # ================ Character I/O ================

  # ---------------- Input ----------------

  def append_input(num : Int32)
    @input_queue << num
  end

  # Assumes input is only fed to us via append_input.
  def get_input
    if @input_queue.size == 0
      a : String? = nil

      print("input: ")
      STDOUT.flush
      a = gets
      while a.nil? || a == ""
        a = gets
      end
      a.as(String).chomp.to_i
    else
      val = @input_queue[0]
      @input_queue = @input_queue[1..] if @input_queue.size > 0
      val
    end
  end

  def flush_input
    @input_queue = [] of Int32
  end

  # ---------------- Output ----------------

  def direct_output_to(stream)
    @output_io = stream
  end

  def append_output(val)
    case @output_io
    when IntcodeComputer
      @output_io.as(IntcodeComputer).append_input(val)
    when IO
      puts(val)
    when Array(Int32)
      @output_io.as(Array(Int32)) << val
    end
  end

  def has_output
    @output_queue.size > 0
  end

  # Returns nil if no output
  def get_output
    return nil if @output_queue.size == 0

    val = @output_queue[0]
    @output_queue = @output_queue[1..]
    val
  end

  # Output is flushed at the start of every `#run`. (Input is not.)
  def flush_output
    @output_queue = [] of Int32
  end

  # ================ Debugging ================

  def trace(val : Bool)
    @trace = val
  end

  def trace_instruction(name, num_params, must_be_addr = -1)
    return unless @trace
    param_strs = (0...num_params).map do |i|
      offset = i + 1
      mode_char = i == must_be_addr ? '@' : mode_char(offset)
      "#{mode_char}#{get(@pc + offset)}"
    end
    puts("#{name}\t#{param_strs.join(", ")}")
  end

  def trace_result(val)
    puts("\t=> #{val}") if @trace
  end

  def do_trace(name, num_params, must_be_addr = -1)
    trace_instruction(name, num_params, must_be_addr)
    val = yield
    trace_result(val)
  end

  # ================ CPU ================

  def parse_opcode_at_pc
    param_mode_num, @opcode = @mem[@pc].divmod(100)
    @param_modes = param_mode_num.to_s.reverse.chars.map do |d|
      d.to_i == 0 ? ParamMode::Indirect : ParamMode::Immediate
    end
  end

  def mode_of_param(offset)
    (offset - 1) < @param_modes.size ? @param_modes[offset - 1] : ParamMode::Indirect
  end

  def mode_char(offset)
    mode_of_param(offset) == ParamMode::Indirect ? '@' : '#'
  end

  def val_of_param(offset)
    param_mode =
      param = @mem[@pc + offset]
    if mode_of_param(offset) == ParamMode::Indirect
      @mem[param]
    else
      param
    end
  end

  def addr_at_param(offset)
    @mem[@pc + offset]
  end
end
