# Packet Scanners

defmodule Y2017.Day13 do
  use Common.File

  @moduledoc """
  Assumes all ranges are either nil or > 1, which is true for my input
  data. Were it false, I'd have to be more careful about checking range
  bounds and changing directions.
  """
  defmodule Layer do
    defstruct [:depth, :range, :loc, :direction]

    def hit_check_cost(l) do
      if at_zero?(l), do: cost(l), else: 0
    end

    @doc """
    Move this layer one step.

    ## Examples

        iex> Y2017.Day13.Layer.step(%Y2017.Day13.Layer{range: nil})
        %Y2017.Day13.Layer{range: nil}

        iex> Y2017.Day13.Layer.step(%Y2017.Day13.Layer{range: 3, loc: 0, direction: :down})
        %Y2017.Day13.Layer{range: 3, loc: 1, direction: :down}

        iex> Y2017.Day13.Layer.step(%Y2017.Day13.Layer{range: 3, loc: 1, direction: :down})
        %Y2017.Day13.Layer{range: 3, loc: 2, direction: :up}

        iex> Y2017.Day13.Layer.step(%Y2017.Day13.Layer{range: 3, loc: 2, direction: :up})
        %Y2017.Day13.Layer{range: 3, loc: 1, direction: :up}

        iex> Y2017.Day13.Layer.step(%Y2017.Day13.Layer{range: 3, loc: 1, direction: :up})
        %Y2017.Day13.Layer{range: 3, loc: 0, direction: :down}

        iex> Enum.reduce((0..4), %Y2017.Day13.Layer{range: 3, loc: 0, direction: :down},
        ...>   fn(_, l) -> Y2017.Day13.Layer.step(l) end)
        %Y2017.Day13.Layer{range: 3, loc: 1, direction: :down}
    """
    def step(l = %Layer{range: nil}) do
      l
    end

    def step(l = %Layer{range: range, loc: loc, direction: :down})
        when loc == range - 2 do
      %{l | loc: range - 1, direction: :up}
    end

    def step(l = %Layer{loc: loc, direction: :down}) do
      %{l | loc: loc + 1}
    end

    def step(l = %Layer{loc: 1, direction: :up}) do
      %{l | loc: 0, direction: :down}
    end

    def step(l = %Layer{loc: loc, direction: :up}) do
      %{l | loc: loc - 1}
    end

    defp cost(%Layer{depth: depth, range: range}), do: depth * range

    defp at_zero?(%Layer{range: nil}), do: false
    defp at_zero?(%Layer{loc: 0}), do: true
    defp at_zero?(_), do: false
  end

  # ================ main ================

  def part1 do
    firewall = read_firewall()
    len = length(Map.keys(firewall))

    {_, cost} =
      0..(len - 1)
      |> Enum.reduce({firewall, 0}, fn i, {firewall, cost} ->
        new_cost = cost + Layer.hit_check_cost(firewall[i])
        {step_firewall(firewall), new_cost}
      end)

    cost
  end

  # 129680 is too low
  def part2 do
    firewall = read_firewall()
    cache = %{0 => firewall}
    max_depth = length(Map.keys(firewall))

    # assumes step 0 is not the answer
    Stream.iterate({cache, 0, -99}, fn {cache, prev_delay, _prev_cost} ->
      delay = prev_delay + 1
      {firewall, cache} = find_or_create(firewall, delay, cache)

      {_, cache, cost} =
        0..(max_depth - 1)
        |> Enum.reduce({firewall, cache, 0}, fn i, {fwall, cache, cost} ->
          new_cost = cost + Layer.hit_check_cost(fwall[i])
          {new_fwall, new_cache} = find_or_create(fwall, delay + i + 1, cache)
          {new_fwall, new_cache, new_cost}
        end)

      {cache, delay, cost}
    end)
    |> Enum.find(fn {_, _, cost} -> cost == 0 end)
    |> elem(1)
  end

  # ================ helpers ================

  # Given firewall, step, and cache, return a tuple containing {firewall at
  # step, possibly modified cache}.
  defp find_or_create(firewall, step, cache) do
    f = cache[step]

    if f do
      {f, cache}
    else
      {prev_f, cache} = find_or_create(firewall, step - 1, cache)
      f = step_firewall(prev_f)
      {f, Map.put(cache, step, f)}
    end
  end

  defp step_firewall(firewall) do
    len = length(Map.keys(firewall))

    0..(len - 1)
    |> Enum.reduce(firewall, fn i, firewall ->
      Map.replace(firewall, i, Layer.step(firewall[i]))
    end)
  end

  defp read_firewall() do
    input_lines()
    |> Enum.map(&parse_line/1)
    |> Enum.reduce(%{}, fn layer, m -> Map.put(m, layer.depth, layer) end)
    |> fill_in_missing_depths
  end

  defp fill_in_missing_depths(m) do
    max_depth = Enum.max(Map.keys(m))

    0..(max_depth - 1)
    |> Enum.reduce(m, fn i, m ->
      Map.put_new(m, i, %Layer{depth: i})
    end)
  end

  defp parse_line(line) do
    [depth, range] =
      line
      |> String.split(": ")
      |> Enum.map(&String.trim/1)
      |> Enum.map(&String.to_integer/1)

    %Layer{depth: depth, range: range, loc: 0, direction: :down}
  end
end
