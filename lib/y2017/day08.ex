# I Heard You Like Registers

defmodule Y2017.Day08 do
  use Common.File

  defmodule Registers do
    defstruct regs: %{}, max_seen: -0x7FFFFFFF
  end

  defmodule Instruction do
    defstruct [:reg, :op, :const, :if]
  end

  def part1 do
    regs = run()
    regs.regs |> Map.values() |> Enum.max()
  end

  def part2 do
    regs = run()
    regs.max_seen
  end

  defp run do
    input_lines()
    |> Enum.map(&parse_line/1)
    |> Enum.reduce(%Registers{}, fn instr, regs -> run_instruction(instr, regs) end)
  end

  defp read(reg, %Registers{regs: regs}), do: Map.get(regs, reg, 0)

  defp write(reg, val, regs) do
    %{
      regs
      | regs: Map.put(regs.regs, reg, val),
        max_seen: if(val > regs.max_seen, do: val, else: regs.max_seen)
    }
  end

  defp run_instruction(inst, regs) do
    if eval(inst.if, regs) do
      eval(inst, regs)
    else
      regs
    end
  end

  defp eval(%Instruction{reg: reg, op: :==, const: const}, regs) do
    read(reg, regs) == const
  end

  defp eval(%Instruction{reg: reg, op: :!=, const: const}, regs) do
    read(reg, regs) != const
  end

  defp eval(%Instruction{reg: reg, op: :>, const: const}, regs) do
    read(reg, regs) > const
  end

  defp eval(%Instruction{reg: reg, op: :>=, const: const}, regs) do
    read(reg, regs) >= const
  end

  defp eval(%Instruction{reg: reg, op: :<, const: const}, regs) do
    read(reg, regs) < const
  end

  defp eval(%Instruction{reg: reg, op: :<=, const: const}, regs) do
    read(reg, regs) <= const
  end

  defp eval(%Instruction{reg: reg, op: :dec, const: const}, regs) do
    write(reg, read(reg, regs) - const, regs)
  end

  defp eval(%Instruction{reg: reg, op: :inc, const: const}, regs) do
    write(reg, read(reg, regs) + const, regs)
  end

  defp parse_line(line) do
    [reg, op, const, "if", if_lhs, if_op, if_const] = String.split(line)

    %Instruction{
      reg: String.to_atom(reg),
      op: String.to_atom(op),
      const: String.to_integer(const),
      if: %Instruction{
        reg: String.to_atom(if_lhs),
        op: String.to_atom(if_op),
        const: String.to_integer(if_const)
      }
    }
  end
end
