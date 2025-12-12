defmodule Common.MapCache do
  @doc """
  Starts the MapCache. Returns same values as `Agent.start_link`.
  """
  def start_link do
    Agent.start_link(fn -> %{} end)
  end

  @doc """
  Stops a MapCache.
  """
  def stop(pid) do
    Agent.stop(pid)
  end

  @doc """
  Gets a value from a `MapCache`. Returns `nil` if no value exists.
  """
  def get(pid, key), do: Agent.get(pid, fn map -> Map.get(map, key) end)

  @doc """
  Gets a value from a `MapCache`. If it exists, returns the value, else
  calls `func`, populates the cache with the returned value, and returns
  that value.

  This operation is not atomic. If multiple processes are using the same
  cache, it's possible for `func` to get called more than once if the key
  has not yet been saved.
  """
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

  @doc """
  Stores `value` at `key`.
  """
  def put(pid, key, value), do: Agent.update(pid, fn map -> Map.put(map, key, value) end)
end
