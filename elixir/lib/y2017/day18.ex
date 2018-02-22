# Duet
defmodule Y2017.Day18 do

  defmodule CoreCPU do
    @moduledoc """
    For inclusion by CPU modules. Implements core CPU behavior.
    """

    def new(snd_func, rcv_func) do
      %{pc: 0, snd_func: snd_func, rcv_func: rcv_func}
    end

    def value_of(_, ref) when is_integer(ref), do: ref

    def value_of(memory, ref) when is_atom(ref), do: Map.get(memory, ref, 0)

    def store(memory, ref, val), do: Map.put(memory, ref, val)

    def pc(memory), do: Map.get(memory, :pc)

    def incr(memory, ref, n \\ 1) do
      Map.put(memory, ref, Map.get(memory, ref) + n)
    end

    def incr_pc(memory, n \\ 1) do
      incr(memory, :pc, n)
    end

    def execute({:set, reg, ref}, memory) do
      store(memory, reg, value_of(memory, ref))
      |> incr_pc()
    end

    def execute({:add, reg, ref}, memory) do
      v1 = value_of(memory, reg)
      v2 = value_of(memory, ref)
      store(memory, reg, v1 + v2)
      |> incr_pc()
    end

    def execute({:mul, reg, ref}, memory) do
      v1 = value_of(memory, reg)
      v2 = value_of(memory, ref)
      store(memory, reg, v1 * v2)
      |> incr_pc()
    end

    def execute({:mod, reg, ref}, memory) do
      v1 = value_of(memory, reg)
      v2 = value_of(memory, ref)
      store(memory, reg, Integer.mod(v1, v2))
      |> incr_pc()
    end

    def execute({:jgz, reg, ref}, memory) do
      delta = if value_of(memory, reg) > 0, do: value_of(memory, ref), else: 1
      incr_pc(memory, delta)
    end

    def execute({:snd, ref}, memory) do
      value_of(memory, :snd_func).({:snd, ref}, memory)
    end

    def execute({:rcv, ref}, memory) do
      value_of(memory, :rcv_func).({:rcv, ref}, memory)
    end
  end

  defmodule CPU1 do
    @moduledoc """
    CPU for part 1.
    """

    import CoreCPU

    def new do
      CoreCPU.new(&do_snd/2, &do_rcv/2)
    end

    def run_until_recovery(program) do
      run_until_recovery(program, CPU1.new())
    end

    defp run_until_recovery(program, memory) do
      # do nothing (i.e. return) if recover register is set
      if value_of(memory, :recover) == 0 do
        new_memory = execute(Enum.at(program, pc(memory)), memory)
        run_until_recovery(program, new_memory)
      else
        memory
      end
    end

    def do_snd({:snd, ref}, memory) do
      store(memory, :played, value_of(memory, ref))
      |> incr_pc()
    end

    def do_rcv({:rcv, ref}, memory) do
      if value_of(memory, ref) != 0 do
        store(memory, :recover, value_of(memory, :played))
      else
        memory
      end
      |> incr_pc()
    end
  end

  # ================ CPU simple looper ================

  defmodule CPU2 do
    @moduledoc """
    CPU for part 1.
    """

    import CoreCPU

    def new(cpu_number) do
      memory = CoreCPU.new(&do_snd/2, &do_rcv/2) |> Map.put(:p, cpu_number)
      pid = spawn(fn -> loop([], memory) end)
      Map.put(memory, :loop_pid, pid)
    end

    def register_other(this_cpu, other_cpu_pid) do
      Map.put(this_cpu, :other_cpu_pid, other_cpu_pid)
    end

    def loop(program, memory) do
      IO.puts "started loop, pid #{inspect(self())}" # DEBUG
      case run_until_receive_or_error(program, memory) do
        {:error, new_memory} ->
          new_memory
        {:receive, new_memory} ->
          receive do
            {:other_cpu_pid, other_pid} ->
              IO.puts "pid #{inspect(self())} received :other_cpu_pid, other pid = #{inspect(other_pid)}" # DEBUG
              Map.put(memory, :other_cpu_pid, other_pid)
              case run_until_receive_or_error(program, memory) do
                {:error, new_memory} ->
                  new_memory
                {:receive, new_memory} ->
                  loop(program, new_memory)
              end
            {:get_other_cpu_pid, response_pid} ->
              send(response_pid, {:other_cpu_pid, value_of(memory, :other_cpu_pid)})
              loop(program, memory)
            {:run, new_program} ->
              loop(new_program, memory)
            {:sent_from_other, val} ->
              {:rcv, reg} = Enum.at(program, pc(memory))
              received = store(memory, reg, val) |> incr_pc()
              case run_until_receive_or_error(program, received) do
                {:error, new_memory} ->
                  new_memory
                {:receive, new_memory} ->
                  loop(program, new_memory)
              end
          end
      end
    end

    def run_until_receive_or_error(program, %{pc: pc} = memory)
    when pc < 0 or pc >= length(program) do
      IO.puts "#{value_of(memory, :p)} run_until_receive_or_error out of bounds" # DEBUG
      {:error, memory}
    end

    def run_until_receive_or_error(program, memory) do
      IO.puts "#{value_of(memory, :p)} run_until_receive_or_error pc #{value_of(memory, :pc)}" # DEBUG
      instruction = Enum.at(program, pc(memory))
      case instruction do
        {:rcv, _ref} ->
          {:receive, memory}
        inst -> run_until_receive_or_error(program, execute(inst, memory))
      end
    end

    defp do_snd({:snd, ref}, memory) do
      IO.puts "#{value_of(memory, :p)} sending to other" # DEBUG
      send(value_of(memory, :loop_pid), {:get_other_cpu_pid, self()})
      IO.puts "waiting for response" # DEBUG
      other_cpu_pid = receive do
        {:other_cpu_pid, pid} ->
          pid |> IO.inspect(label: "#{value_of(memory, :p)} received other pid #{inspect(pid)}") # DEBUG
      end
      send(other_cpu_pid, {:sent_from_other, value_of(memory, ref)})
      memory
      |> incr(:send_count)
      |> incr_pc()
    end

    defp do_rcv({:rcv, _ref}, memory) do
      IO.puts "#{value_of(memory, :p)} do_rcv" # DEBUG
      memory
    end
  end

  # ================ end CPU GenServer ================

  use Common.File

  def part1(program \\ read_program()) do
    CPU1.run_until_recovery(program)
    |> Map.get(:recover)
  end

  def test_part1 do
    part1(read_test1_program())
  end

  def part2(program \\ read_program()) do
    cpu1 = CPU2.new(0)
    cpu2 = CPU2.new(1)
    cpu1 = CPU2.register_other(cpu1, CoreCPU.value_of(cpu2, :loop_pid))
    cpu2 = CPU2.register_other(cpu2, CoreCPU.value_of(cpu1, :loop_pid))

    CPU2.run_until_receive_or_error(program, cpu1)
    cpu2 = CPU2.run_until_receive_or_error(program, cpu2)
    CoreCPU.value_of(cpu2, :send_count)
  end

  def test_part2 do
    part2(read_test2_program())
  end

  # ================ helpers ================

  defp read_program() do
    input_lines()
    |> Enum.map(&parse_instruction/1)
  end

  defp read_test1_program() do
    [
      "set a 1",
      "add a 2",
      "mul a a",
      "mod a 5",
      "snd a",
      "set a 0",
      "rcv a",
      "jgz a -1",
      "set a 1",
      "jgz a -2"
    ]
    |> Enum.map(&parse_instruction/1)
  end

  defp read_test2_program() do
    [
      "snd 1",
      "snd 2",
      "snd p",
      "rcv a",
      "rcv b",
      "rcv c",
      "rcv d"
    ]
    |> Enum.map(&parse_instruction/1)
  end

  defp parse_instruction(line) do
    tokens = String.split(line)

    case tokens do
      [op, reg] ->
        {String.to_atom(op), String.to_atom(reg)}

      [op, reg, dest] ->
        case Integer.parse(dest) do
          {n, _} when is_integer(n) ->
            {String.to_atom(op), String.to_atom(reg), n}

          :error ->
            {String.to_atom(op), String.to_atom(reg), String.to_atom(dest)}
        end
    end
  end
end
