# Some Assembly Required

defmodule Y2015.Day07 do
  use Common.File
  import Bitwise

  def run1, do: run(& &1)

  def run2, do: run(&reset_and_rerun/1)

  defp run(modify) do
    default_input_path()
    |> File.stream!()
    |> tokenize
    |> reorder
    |> modify.()
    |> execute
    |> Map.get(:a)
  end

  # ================ tokenization ================

  defp tokenize(statements) do
    statements |> Enum.map(&tokenize_statement/1)
  end

  # Given "foo bar -> target" returns ["target", "foo", "bar"]
  defp tokenize_statement(s) do
    [target | ["->" | rest]] = s |> String.split() |> Enum.reverse()

    args =
      rest
      |> Enum.reverse()
      |> Enum.map(fn
        <<c>> <> _ = s when c >= ?0 and c <= ?9 -> String.to_integer(s)
        s -> String.to_atom(s)
      end)

    [String.to_atom(target) | args]
  end

  # ================ statement reordering ================

  # Reorders a list of statement lists to eliminate dependencies. Each
  # statement list is of the form
  #
  # - [target, value]                   (assignment)
  # - [target, value, :BINOP, value]    (binary operation and assignment)
  # - [target, :NOT, value]             (negation and assignment)
  defp reorder(statements) do
    dependencies =
      statements
      |> Enum.reduce(%{}, fn
        [target, value], m ->
          m |> add_dependency(target, value)

        [target, :NOT, value], m ->
          m |> add_dependency(target, value)

        [target, value1, _, value2], m ->
          m |> add_dependency(target, value1) |> add_dependency(target, value2)
      end)

    reorder(statements, dependencies |> Map.to_list(), [])
  end

  defp add_dependency(m, target, value) do
    if const?(value) do
      Map.put_new(m, target, [])
    else
      Map.put(m, target, [value | Map.get(m, target, [])])
    end
  end

  defp const?(i) when is_integer(i), do: true
  defp const?(_), do: false

  defp reorder([], [], reordered), do: Enum.reverse(reordered)

  defp reorder(statements, dependencies, reordered) do
    free_targets =
      dependencies
      |> Enum.filter(fn
        {_, []} -> true
        _ -> false
      end)
      |> Enum.map(fn {k, _} -> k end)

    {nd_statements, d_statements} =
      statements
      |> Enum.split_with(fn [target | _] -> Enum.member?(free_targets, target) end)

    reorder(
      d_statements,
      remove_satisfied_dependencies(dependencies, free_targets),
      Enum.concat(nd_statements, reordered)
    )
  end

  defp remove_satisfied_dependencies(dependencies, free_targets) do
    dependencies
    |> Enum.map(fn
      {_, []} -> nil
      {k, vs} -> {k, vs |> delete_all(free_targets)}
    end)
    |> Enum.filter(& &1)
  end

  defp delete_all(vs, []), do: vs
  defp delete_all(vs, [h | t]), do: delete_all(List.delete(vs, h), t)

  defp reset_and_rerun(statements) do
    # strip first const assignment to b
    statements
    |> Enum.concat([[:b, :a], [:a, 0]])
    |> Enum.concat(tl(statements))
  end

  # ================ execution ================

  defp execute(statements) do
    statements |> Enum.reduce(%{}, &execute/2)
  end

  defp execute([target, :NOT, var], memory) do
    memory |> Map.put(target, bnot(value_of(memory, var)) &&& 0xFFFF)
  end

  defp execute([target, var1, :AND, var2], memory) do
    memory |> Map.put(target, value_of(memory, var1) &&& value_of(memory, var2))
  end

  defp execute([target, var1, :OR, var2], memory) do
    memory |> Map.put(target, bor(value_of(memory, var1), value_of(memory, var2)))
  end

  defp execute([target, var1, :LSHIFT, var2], memory) do
    memory |> Map.put(target, bsl(value_of(memory, var1), value_of(memory, var2)) &&& 0xFFFF)
  end

  defp execute([target, var1, :RSHIFT, var2], memory) do
    memory |> Map.put(target, bsr(value_of(memory, var1), value_of(memory, var2)))
  end

  defp execute([target, value], memory) do
    memory |> Map.put(target, value_of(memory, value))
  end

  defp value_of(_, i) when is_integer(i), do: i
  defp value_of(memory, var), do: Map.get(memory, var)
end
