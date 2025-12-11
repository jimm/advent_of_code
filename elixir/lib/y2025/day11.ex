# ================ Reactor ================

defmodule Y2025.Day11 do
  def part1(_ctx, lines) do
    conns = parse_connections(lines)
    num_paths_you_to_out(conns)
  end

  def part2(_ctx, lines) do
    conns = parse_connections(lines)

    num_paths_through_dac_and_fft(conns)
  end

  # ================ helpers ================

  defp num_paths_you_to_out(conns) do
    num_paths_from_to(:you, :out, conns)
  end

  # We first count the number of paths from dac->fft or fft->dac (one of
  # those will be zero). That tells us which needs to come first. Then we
  # count the paths from :svr (the start) to that, and from the other to
  # :out. We have three path counts. Multiply them, and that's our answer.
  def num_paths_through_dac_and_fft(conns) do
    # one of these two, dac->fft or fft->dac, will be zero
    dac_to_fft = num_paths_from_to(:dac, :fft, conns)

    fft_to_dac =
      if dac_to_fft == 0 do
        num_paths_from_to(:fft, :dac, conns)
      else
        0
      end

    if dac_to_fft > 0 do
      before_count = num_paths_from_to(:svr, :dac, conns)
      after_count = num_paths_from_to(:fft, :out, conns)
      before_count * dac_to_fft * after_count
    else
      before_count = num_paths_from_to(:svr, :fft, conns)
      after_count = num_paths_from_to(:dac, :out, conns)
      before_count * fft_to_dac * after_count
    end
  end

  # Returns number of paths from `from` to :out. Uses `cache`. Assumes no
  # cycles.
  defp num_paths_from_to(from, to, conns) do
    {:ok, cache} = Agent.start_link(fn -> %{} end, name: __MODULE__)
    num = num_paths_from_to(from, to, conns, cache)
    Agent.stop(cache)
    num
  end

  defp num_paths_from_to(node, node, _, _), do: 1

  defp num_paths_from_to(:out, to, _, _) when to != :out, do: 0

  defp num_paths_from_to(from, to, conns, cache) do
    children = conns[from]

    if children == nil do
      1
    else
      children
      |> Enum.map(fn child ->
        cache_key = [child, to]
        cached = Agent.get(cache, fn state -> Map.get(state, cache_key) end)

        if cached == nil do
          cached = num_paths_from_to(child, to, conns, cache)
          Agent.update(cache, fn state -> Map.put(state, cache_key, cached) end)
          cached
        else
          cached
        end
      end)
      |> Enum.sum()
    end
  end

  defp parse_connections(lines) do
    lines
    |> Enum.map(fn line ->
      [node, others] = String.split(line, ": ")
      outputs = others |> String.split(" ") |> Enum.map(&String.to_atom/1)
      {String.to_atom(node), outputs}
    end)
    |> Enum.into(%{})
  end
end
