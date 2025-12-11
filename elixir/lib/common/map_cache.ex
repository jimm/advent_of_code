defmodule Common.MapCache do
  def start_link do
    Agent.start_link(fn -> %{} end)
  end

  def stop(pid) do
    Agent.stop(pid)
  end

  def get(pid, key), do: Agent.get(pid, fn map -> Map.get(map, key) end)

  def get_lazy(pid, key, func) do
    val = get(pid, key)

    if val == nil do
      val = func.()
      put(pid, key, val)
      val
    else
      val
    end
  end

  def put(pid, key, value), do: Agent.update(pid, fn map -> Map.put(map, key, value) end)
end
