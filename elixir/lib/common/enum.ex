defmodule Common.Enum do
  @moduledoc """
  Enumerable utilities, permutation, and combination.
  """

  @doc """
  Parallel map.
  """
  def pmap(coll, fun) do
    coll
    |> Enum.map(&Task.async(fn -> fun.(&1) end))
    |> Enum.map(&Task.await/1)
  end

  @doc """
  N things taken N at a time. See also `permutations/2`.

  ## Examples

      iex> Common.Enum.permutations([1, 2, 3]) |> Enum.sort
      [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]
  """
  def permutations(xs), do: permutations(xs, length(xs))

  @doc """
  N things taken K at a time.

  ## Examples

      iex> Common.Enum.permutations([1, 2, 3], 3) |> Enum.sort
      [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]

      iex> Common.Enum.permutations([1, 2, 3], 2) |> Enum.sort
      [[1, 2], [1, 3], [2, 1], [2, 3], [3, 1], [3, 2]]

      iex> alias Common.Enum, as: CE
      iex> (0..5) |> Enum.map(&(CE.permutations([1, 2, 3, 4, 5], &1) |> length))
      [1, 5, 20, 60, 120, 120]

  Equal values are treated as distinct.

      iex> Common.Enum.permutations([1, 2, 3, 4, 3], 2)
      [[1, 2], [1, 3], [1, 4], [1, 3],
       [2, 1], [2, 3], [2, 4], [2, 3],
       [3, 1], [3, 2], [3, 4], [3, 3],
       [4, 1], [4, 2], [4, 3], [4, 3],
       [3, 1], [3, 2], [3, 4], [3, 3]]
  """
  def permutations(_, 0), do: [[]]
  def permutations(xs, 1), do: xs |> Enum.map(&[&1])

  def permutations(xs, k) do
    for x <- xs,
        y <- permutations(List.delete(xs, x), k - 1) do
      [x | y]
    end
  end

  @doc """
  All combinations of K elements of the given `Enumerable`.

      iex> Common.Enum.combinations([1, 2, 3], 3) |> Enum.sort
      [[1, 2, 3]]

      iex> Common.Enum.combinations([1, 2, 3, 4], 2) |> Enum.sort
      [[1, 2], [1, 3], [1, 4],
       [2, 3], [2, 4],
       [3, 4]]

      iex> alias Common.Enum, as: CE
      iex> (0..5) |> Enum.map(fn k -> CE.combinations([1, 2, 3, 4, 5], k) |> length end)
      [1, 5, 10, 10, 5, 1]

  Equal values are treated as distinct. 

      iex> Common.Enum.combinations([1, 3, 3, 4], 2) |> Enum.sort
      [[1, 3], [1, 3], [1, 4],
       [3, 3], [3, 4],
       [3, 4]]
  """
  def combinations(_, 0), do: [[]]
  def combinations(xs, 1), do: xs |> Enum.map(&[&1])
  def combinations(xs, k) when k >= length(xs), do: [xs]

  def combinations([h | t], k) do
    with_h =
      for cs <- combinations(t, k - 1) do
        [h | cs]
      end

    with_h ++ combinations(t, k)
  end

  @doc """
  Returns "windows" of values from an `Enumerable` of length `n`.

  ## Examples

      iex> Common.Enum.window([1, 2, 3, 4, 5], 2)
      [[1, 2], [2, 3], [3, 4], [4, 5]]

      iex> Common.Enum.window([1], 4)
      []

      iex> Common.Enum.window([], 4)
      []
  """
  def window(xs, n), do: window(xs, n, [])

  def window([], _, _), do: []

  def window(xs, n, results) when length(xs) < n, do: Enum.reverse(results)

  def window(xs, n, results) do
    window(tl(xs), n, [Enum.take(xs, n) | results])
  end
end
