defmodule Common.Context do
  @moduledoc """
  The context passed in to solution part functions.
  """

  defstruct [:year, :day, :part, :module, test: false, debug: false]
end
