# Spinlock

defmodule Y2017.Day17 do
  @input 328
  @last_insert 2017
  @test_input 3

  # Circular buffer. An agent holds the list of values, the current
  # index, and the length of the list (an optimization).
  #
  # We only ever need one circular buffer in this challenge, so the
  # functions don't take any args that identify which agent we're using.
  defmodule CircularBuffer do
    def init(initial_list) do
      Agent.start_link(fn -> {initial_list, 0, length(initial_list)} end)
    end

    def forward(pid, n) do
      Agent.update(pid, fn {list, pos, len} ->
        {list, Integer.mod(pos + n, len), len}
      end)
    end

    @doc "Inserts `val` after the current location."
    def insert_after(pid, val) do
      Agent.update(pid, fn {list, pos, len} ->
        {List.insert_at(list, pos + 1, val), pos + 1, len + 1}
      end)
    end

    # optimization
    def forward_and_insert_after(pid, n, val) do
      Agent.update(pid, fn {list, pos, len} ->
        pos = Integer.mod(pos + n, len)
        {List.insert_at(list, pos + 1, val), pos + 1, len + 1}
      end)
    end

    def to_list(pid) do
      {list, _, _} = Agent.get(pid, & &1)
      list
    end
  end

  # ================ end CircularBuffer ================

  alias CircularBuffer, as: CB

  def part1 do
    spinlock(@input, @last_insert)
    |> value_after(@last_insert)
  end

  def test_part1 do
    spinlock(@test_input, @last_insert)
    |> value_after(@last_insert)
  end

  def part2 do
    part2_spinlock(@input, 50_000_000)
  end

  # ================ part 1 ================

  defp spinlock(steps, last_insert) do
    {:ok, cbuf} = CB.init([0])

    1..last_insert
    |> Enum.each(fn i ->
      CB.forward_and_insert_after(cbuf, steps, i)
    end)

    CB.to_list(cbuf)
  end

  def value_after([val, answer | _], val), do: answer
  def value_after([_ | t], val), do: value_after(t, val)

  # ================ part 2 ================

  defp part2_spinlock(steps, last_insert) do
    {slot_1, _, _} =
      1..last_insert
      |> Enum.reduce({nil, 0, 1}, fn i, {slot_1, pos, len} ->
        pos = Integer.mod(pos + steps, len)
        {if(pos == 0, do: i, else: slot_1), pos + 1, len + 1}
      end)

    slot_1
  end
end
