# I Heard You Like Registers

defmodule Y2017.Day08 do
  alias Common.File, as: CF

  defmodule Instruction do
    defstruct [:reg, :op, :const, :if]
  end

  def part1 do
    __MODULE__
    |> CF.default_input_path
    |> CF.lines
    |> Enum.map(&parse_line/1)
    |> Enum.reduce(%{}, fn(instr, regs) -> run_instruction(instr, regs) end)
    |> Map.values
    |> Enum.max
  end

  defp val(reg, regs), do: Map.get(regs, reg, 0)

  defp run_instruction(inst, regs) do
    if eval(inst.if, regs) do
      eval(inst, regs)
    else
      regs
    end
  end

  defp eval(%Instruction{reg: reg, op: :==,   const: const}, regs) do
    val(reg, regs) == const
  end
  defp eval(%Instruction{reg: reg, op: :!=,   const: const}, regs) do
    val(reg, regs) != const
  end
  defp eval(%Instruction{reg: reg, op: :>,   const: const}, regs) do
    val(reg, regs) > const
  end
  defp eval(%Instruction{reg: reg, op: :>=,  const: const}, regs) do
    val(reg, regs) >= const
  end
  defp eval(%Instruction{reg: reg, op: :<,   const: const}, regs) do
    val(reg, regs) < const
  end
  defp eval(%Instruction{reg: reg, op: :<=,  const: const}, regs) do
    val(reg, regs) <= const
  end
  defp eval(%Instruction{reg: reg, op: :dec, const: const}, regs) do
    Map.put(regs, reg, val(reg, regs) - const)
  end
  defp eval(%Instruction{reg: reg, op: :inc, const: const}, regs) do
    Map.put(regs, reg, val(reg, regs) + const)
  end

  defp parse_line(line) do
    [reg, op, const, "if", if_lhs, if_op, if_const] = String.split(line)
    %Instruction{reg: String.to_atom(reg),
                 op: String.to_atom(op),
                 const: String.to_integer(const),
                 if: %Instruction{reg: String.to_atom(if_lhs),
                                  op: String.to_atom(if_op),
                                  const: String.to_integer(if_const)}}
  end
end
