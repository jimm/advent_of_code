# Duet
defmodule Y2017.Day18 do

  # ================ CPU simple looper ================

  defmodule CPU do
    def loop(state) do
      case run_until_receive_or_error(state) do
        :error ->
          :error
        new_state ->
          loop(new_state)
      end
    end

    def run_until_receive_or_error(state) do
    end
  end

  # ================ end CPU GenServer ================

  use Common.File

  def part1 do
    program = read_program()
    registers = Map.new()
    # returns registers
    run_until_recovery(program, registers)
    |> value_of_special(:recover)
  end

  def test_part1 do
    program = read_test1_program()
    registers = Map.new()
    # returns registers
    run_until_recovery(program, registers)
    |> value_of(:recover)
  end

  # ================ part 1 ================

  def run_until_recovery(program, registers) do
    run_until_recovery(program, 0, registers)
  end

  def run_until_recovery(program, pc, registers) do
    # do nothing (i.e. return) if recover register is set
    if value_of_special(registers, :recover) == nil do
      {new_pc, new_regs} = execute(Enum.at(program, pc), pc, registers)
      run_until_recovery(program, new_pc, new_regs)
    else
      registers
    end
  end

  # ================ execution ================

  defp value_of(_, ref) when is_integer(ref), do: ref

  defp value_of(regs, ref) when is_atom(ref), do: Map.get(regs, ref, 0)

  defp value_of_special(regs, ref), do: Map.get(regs, ref)

  defp store(regs, ref, val), do: Map.put(regs, ref, val)

  defp execute({:set, reg, ref}, pc, regs) do
    {pc + 1, store(regs, reg, value_of(regs, ref))}
  end

  defp execute({:add, reg, ref}, pc, regs) do
    v1 = value_of(regs, reg)
    v2 = value_of(regs, ref)
    {pc + 1, store(regs, reg, v1 + v2)}
  end

  defp execute({:mul, reg, ref}, pc, regs) do
    v1 = value_of(regs, reg)
    v2 = value_of(regs, ref)
    {pc + 1, store(regs, reg, v1 * v2)}
  end

  defp execute({:mod, reg, ref}, pc, regs) do
    v1 = value_of(regs, reg)
    v2 = value_of(regs, ref)
    {pc + 1, store(regs, reg, Integer.mod(v1, v2))}
  end

  defp execute({:jgz, reg, ref}, pc, regs) do
    new_pc =
      if value_of(regs, reg) > 0 do
        pc + value_of(regs, ref)
      else
        pc + 1
      end

    {new_pc, regs}
  end

  # ==== send and receive ====

  defp execute({:snd, ref}, pc, regs) do
    {pc + 1, store(regs, :played, value_of(regs, ref))}
  end

  defp execute({:rcv, ref}, pc, regs) do
    new_regs =
      if value_of(regs, ref) != 0 do
        store(regs, :recover, value_of(regs, :played))
      else
        regs
      end

    {pc + 1, new_regs}
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
