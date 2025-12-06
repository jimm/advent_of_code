# ================ Cafeteria ================

defmodule Y2025.Day05 do
  def part1(_ctx, lines) do
    {ranges, ids} = parse(lines)

    ids
    |> Enum.filter(fn id -> Enum.any?(ranges, &(id in &1)) end)
    |> length
  end

  def part2(_ctx, lines) do
    {ranges, _} = parse(lines)

    consolidate_ranges(tl(ranges), length(ranges), [hd(ranges)])
    |> Enum.map(&Range.size/1)
    |> Enum.sum()
  end

  # ================ helpers ================

  def consolidate_ranges([], num_ranges, consolidated) do
    if num_ranges == length(consolidated) do
      # no changes, so we're done
      Enum.uniq(consolidated)
    else
      # changes were made, we need to try again
      consolidate_ranges(tl(consolidated), length(consolidated), [hd(consolidated)])
    end
  end

  def consolidate_ranges([range | ranges], num_ranges, consolidated) do
    # For each of the ranges consolidated so far, compare it to the incoming
    # range. If we can, merge the two.
    consolidate_ranges(
      ranges,
      num_ranges,
      Enum.reduce(consolidated, [], fn crange, acc ->
        merged = merge_ranges(range, crange)
        Enum.uniq(acc ++ merged)
      end)
    )
  end

  # Merges r1 and r2 if possible and returns an array with one or two
  # ranges.
  def merge_ranges(r1, r2) do
    # Sort by beginning so we have fewer cases to handle
    [r1, r2] = if r1.first > r2.first, do: [r2, r1], else: [r1, r2]

    if r1.last < r2.first - 1 do
      [r1, r2]
    else
      [r1.first..Enum.max([r1.last, r2.last])]
    end
  end

  # Returns a tuple containing a list of ranges and a list of ids
  defp parse(lines) do
    lines
    |> Enum.reduce({[], []}, fn line, {ranges, ids} ->
      case String.split(line, "-") do
        [min, max] ->
          range = String.to_integer(min)..String.to_integer(max)
          {[range | ranges], ids}

        [""] ->
          {ranges, ids}

        [num] ->
          id = String.to_integer(num)
          {ranges, [id | ids]}
      end
    end)
  end
end
