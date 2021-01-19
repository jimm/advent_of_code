# The Ideal Stocking Stuffer

defmodule Y2015.Day04 do
  @key "iwrupvqb"

  def run1 do
    mine_coin(fn hash ->
      <<first_n :: size(20), _ :: size(4), _ :: binary>> = hash
      first_n == 0
    end)
  end

  def run2 do
    mine_coin(fn hash ->
      <<first_n :: size(24), _ :: binary>> = hash
      first_n == 0
    end)
  end

  def mine_coin(f, key \\ @key) do
    Stream.iterate(1, &(&1 + 1))
    |> Enum.find(fn i -> f.(:crypto.hash(:md5, "#{key}#{i}")) end)
  end
end
