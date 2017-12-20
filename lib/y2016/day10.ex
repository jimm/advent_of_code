defmodule Y2016.Day10 do
  @empty_bot {nil, nil, nil, nil}

  use Common.File

  defmodule Factory do
    defstruct bots: %{}, outputs: %{}
  end

  # Factory is a map from bot number => bot
  # Bot is {low instruction, high instruction, val1, val2}
  # Instruction is either {:give, botnum} or {:output, binnum}
  def run1(file \\ nil, comp1 \\ 17, comp2 \\ 61) do
    init_factory(file)
    |> run_bots_until(fn factory -> any_bot_holding?(factory, comp1, comp2) end)
    |> bot_holding(comp1, comp2)
  end

  def run2(file \\ nil) do
    factory =
      init_factory(file)
      |> run_bots
    os = factory.outputs
    Map.get(os, 0) * Map.get(os, 1) * Map.get(os, 2)
  end

  # ================ running the factory ================

  defp run_bots_until(factory, f) do
    Stream.iterate(factory, &run_one_step/1)
    |> Stream.drop_while(&(!done?(&1, f)))
    |> Enum.take(1)
    |> hd
  end

  defp run_bots(factory) do
    Stream.iterate(factory, &run_one_step/1)
    |> Stream.drop_while(&(!done?(&1, fn _ -> false end)))
    |> Enum.take(1)
    |> hd
  end

  defp run_one_step(factory) do
    factory.bots
    |> Enum.filter(fn {_bnum, bot} -> bot_ready?(bot) end)
    |> Enum.reduce(factory, fn {bnum, bot}, factory -> run_bot(factory, bnum, bot) end)
  end

  defp run_bot(factory, bnum, {i1, i2, v1, v2}) do
    factory
    |> run_instruction(i1, min(v1, v2))
    |> run_instruction(i2, max(v1, v2))
    |> empty_bot(bnum)
  end

  defp run_instruction(factory, {:output, onum}, val) do
    %{factory | outputs: Map.put(factory.outputs, onum, val)}
  end
  defp run_instruction(factory, {:bot, bnum}, val) do
    bot = Map.get(factory.bots, bnum)
    new_bot = bot_gets_value(bot, val)
    %{factory | bots: Map.put(factory.bots, bnum, new_bot)}
  end

  defp empty_bot(factory, bnum) do
    {i1, i2, _, _} = Map.get(factory.bots, bnum)
    %{factory | bots: Map.put(factory.bots, bnum, {i1, i2, nil, nil})}
  end

  defp bot_ready?({_, _, v1, v2}) when v1 != nil and v2 != nil, do: true
  defp bot_ready?(_), do: false

  defp bot_empty?({_, _, nil, nil}), do: true
  defp bot_empty?(_), do: false

  defp done?(factory, stopping_func) do
    Enum.all?(factory.bots, fn {_, bot} -> bot_empty?(bot) end) || stopping_func.(factory)
  end

  # ================ run1 helpers ================

  defp bot_holding(factory, v1, v2) do
    botlist =
      factory.bots
      |> Enum.drop_while(fn
        {_, {_, _, ^v1, ^v2}} -> false
        {_, {_, _, ^v2, ^v1}} -> false
        _ -> true
      end)
      |> Enum.take(1)
    case botlist do
      [] -> nil
      [bot|_] -> bot
    end
  end

  defp any_bot_holding?(factory, v1, v2) do
    bot_holding(factory, v1, v2) != nil
  end

  # ================ initialization ================

  defp init_factory(file) do
    bots =
      (file || default_input_path())
      |> input_lines
      |> Enum.map(&String.split/1)
      |> Enum.reduce(%{}, (fn cmd, factory -> init_bot(factory, cmd) end))
    %Factory{bots: bots}
  end

  defp init_bot(factory, words) do
    case words do
      ["value", v, "goes", "to", "bot", b] ->
        update_bot(factory, b, &(bot_gets_value(&1, String.to_integer(v))))
      ["bot", b, "gives", "low", "to", ldest, lnum, "and", "high", "to", hdest, hnum] ->
        update_bot(factory, b, &(bot_gives(&1, ldest, lnum, hdest, hnum)))
    end
  end

  defp update_bot(factory, b, f) do
    bnum = String.to_integer(b)
    bot = Map.get(factory, bnum, @empty_bot)
    new_bot = f.(bot)
    Map.put(factory, bnum, new_bot)
  end

  defp bot_gets_value({i1, i2, v1, v2}, v) do
    case {v1, v2} do
      {nil, nil} -> {i1, i2, v, nil}
      {n, nil} -> {i1, i2, n, v}
    end
  end

  defp bot_gives({_, _, v1, v2}, ldest, lnum, rdest, rnum) do
    {{String.to_atom(ldest), String.to_integer(lnum)},
     {String.to_atom(rdest), String.to_integer(rnum)},
     v1, v2}
  end
end

# Y2016.Day10.run1
# # => 47


