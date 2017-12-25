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

    @doc """
    Returns true if ths layer is at the top (position zero) at the specified
    step. Periodicity is (range + (range - 2)).
    """
    def at_top_at_step?(_, 0), do: true
    def at_top_at_step?(%Layer{range: nil}, _), do: false

    def at_top_at_step?(%Layer{range: range}, step) do
      Integer.mod(step, range + range - 2) == 0
    end

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

  # 129680 is too low Question: is (the least common multiple of all of the
  # ranges) + 1 the answer?
  # 586297958400 is too high
  #
  # Don't have to simulate. Simply find out if on step (N + delay), the Nth
  # level is at the top.
  def part2 do
    firewall = read_firewall()
    len = length(Map.keys(firewall))

    # integer delay starts
    Stream.iterate(0, &(&1 + 1))
    |> Enum.find(&no_hits?(firewall, len, &1))
  end

  # ================ part 2 ================

  defp no_hits?(firewall, len, delay) do
    hit_at =
      0..(len - 1)
      |> Enum.find(fn i ->
        layer = Map.get(firewall, i)
        Layer.at_top_at_step?(layer, delay + i)
      end)

    hit_at == nil
  end

  # ================ helpers ================

  defp step_firewall(firewall) do
    len = length(Map.keys(firewall))

    0..(len - 1)
    |> Enum.reduce(firewall, fn i, firewall ->
      Map.put(firewall, i, Map.get(firewall, i, Layer.step(firewall[i])))
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
