defmodule Y2016.Day19 do
  @input 3_001_330

  defmodule Elf do
    use GenServer

    def num_presents(pid) do
      GenServer.call(pid, :num_presents)
    end

    def turn(pid) do
      GenServer.call(pid, :num_presents)
    end

    def done(pid) do
      GenServer.call(pid, :num_presents)
    end

    def handle_call(:num_presents, _from, {_, num_presents, _} = state) do
      {:reply, num_presents, state}
    end

    def handle_call(:turn, _from, {prev_elf, num_presents, next_elf}) do
      num_presents = num_presents + Elf.num_presents(prev_elf)
      prev_elf = Elf.done(prev_elf)
      {:reply, next_elf, {prev_elf, num_presents, next_elf}}
    end

    def handle_call(:done, _from, {_, prev_elf, _}) do
      {:reply, prev_elf, nil}
    end
  end

  def run1(input \\ @input) do
    first_elf = build_elves(input)
  end

  defp build_elves(input) do
    # TODO
    nil
  end
end

# Y2016.Day19.run1
# #=>

# Y2016.Day19.run2
# #=>
