# ================ Reactor ================

defmodule Y2025.Day11 do
  alias Common.MapCache, as: Cache

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
    {:ok, cache} = Cache.start_link()
    num = num_paths_from_to(from, to, conns, cache)
    Cache.stop(cache)
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
        Cache.get_lazy(cache, [child, to], fn -> num_paths_from_to(child, to, conns, cache) end)
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
