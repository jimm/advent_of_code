# No Such Thing as Too Much

defmodule Y2015.Day17 do
  @liters 150

  alias Common.Set

  def part1(_ctx, lines) do
    liters = @liters

    containers =
      lines
      |> Enum.map(&(&1 |> String.trim() |> String.to_integer()))

    fit_count(containers, liters)
  end

  # min container count
  def part2(_ctx, lines) do
    liters = @liters

    containers =
      lines
      |> Enum.map(&(&1 |> String.trim() |> String.to_integer()))

    min_fit_count(containers, liters)
  end

  @doc """
      iex> Y2015.Day17.fit_count([20, 15, 10, 5, 5], 25)
      4
  """
  def fit_count(containers, liters) do
    fit_count(containers, length(containers), liters, 0)
  end

  def fit_count(_, 0, _, count), do: count

  def fit_count(containers, k, liters, count) do
    matches =
      Set.combinations(containers, k)
      |> Enum.filter(fn perm -> liters == Enum.sum(perm) end)
      |> length

    fit_count(containers, k - 1, liters, count + matches)
  end

  def min_fit_count(containers, liters) do
    min_fit_count(containers, liters, 1)
  end

  def min_fit_count(containers, _, k) when k > length(containers), do: 0

  def min_fit_count(containers, liters, k) do
    combis = Set.combinations(containers, k)

    matches =
      combis
      |> Enum.filter(fn perm -> liters == Enum.sum(perm) end)

    len = length(matches)

    if len > 0 do
      len
    else
      min_fit_count(containers, liters, k + 1)
    end
  end
end
