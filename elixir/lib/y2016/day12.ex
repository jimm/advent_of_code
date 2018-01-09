defmodule Y2016.Day12 do
  use Common.File

  def run1(file \\ nil) do
    run(file, [0, 0, 0, 0])
  end

  def run2(file \\ nil) do
    run(file, [0, 0, 1, 0])
  end

  defp run(file, registers) do
    instructions = (file || default_input_path()) |> input_lines
    # register 1
    run_simulator(instructions, 0, registers)
    |> hd
  end

  def run_simulator(instructions, pc, [a, b, c, d]) when pc >= length(instructions) do
    [a, b, c, d]
  end

  def run_simulator(instructions, pc, registers) do
    instruction = Enum.at(instructions, pc)
    {new_pc, new_registers} = run_instruction(String.split(instruction), pc, registers)
    run_simulator(instructions, new_pc, new_registers)
  end

  def run_instruction(["cpy", x, y], pc, registers) when x in ["a", "b", "c", "d"] do
    {pc + 1, set_register(registers, y, get_register(registers, x))}
  end

  def run_instruction(["cpy", x, y], pc, registers) do
    {pc + 1, set_register(registers, y, String.to_integer(x))}
  end

  def run_instruction(["inc", x], pc, registers) do
    {pc + 1, set_register(registers, x, get_register(registers, x) + 1)}
  end

  def run_instruction(["dec", x], pc, registers) do
    {pc + 1, set_register(registers, x, get_register(registers, x) - 1)}
  end

  def run_instruction(["jnz", x, offset], pc, registers) when x in ["a", "b", "c", "d"] do
    val = get_register(registers, x)
    do_jnz(val, offset, pc, registers)
  end

  def run_instruction(["jnz", x, offset], pc, registers) do
    val = String.to_integer(x)
    do_jnz(val, offset, pc, registers)
  end

  def do_jnz(val, offset, pc, registers) do
    offset = String.to_integer(offset)
    new_pc = if val == 0, do: pc + 1, else: pc + offset
    {new_pc, registers}
  end

  defp get_register([a, _, _, _], "a"), do: a
  defp get_register([_, b, _, _], "b"), do: b
  defp get_register([_, _, c, _], "c"), do: c
  defp get_register([_, _, _, d], "d"), do: d

  defp set_register([_, b, c, d], "a", val), do: [val, b, c, d]
  defp set_register([a, _, c, d], "b", val), do: [a, val, c, d]
  defp set_register([a, b, _, d], "c", val), do: [a, b, val, d]
  defp set_register([a, b, c, _], "d", val), do: [a, b, c, val]
end

# Y2016.Day12.run1
# Y2016.Day12.run2
