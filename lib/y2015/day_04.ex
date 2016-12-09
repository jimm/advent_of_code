defmodule Y2015.Day04 do
  @key "iwrupvqb"

  def mine_coin(f, key \\ @key) do
    Stream.iterate(1, &(&1+1))
    |> Stream.map(fn(i) ->
      <<a, b, c, _ :: binary>> = :crypto.hash(:md5, "#{key}#{i}")
      {i, {a, b, c}}
    end)
    |> Stream.filter(f)
    |> Enum.take(1)
  end
end

# # five
# Y2015.Day04.mine_coin(fn {_, {a, b, c}}
#         when is_integer(a) and a == 0
#          and is_integer(b) and b == 0
#          and is_integer(c) and c < 16 -> true
#       _ -> false
#     end)

# six
# Y2015.Day04.mine_coin(fn {_, {a, b, c}}
#         when is_integer(a) and a == 0
#          and is_integer(b) and b == 0
#          and is_integer(c) and c == 0 -> true
#       _ -> false
#     end)
