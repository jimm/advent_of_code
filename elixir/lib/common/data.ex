defmodule Common.Data do
  @doc """

  Returns the path to the data file, using `part_number` instead of
  `ctx.part`. If `part_number` is nil, the file name won't contain the part
  number.
  """
  def data_file_path(ctx, part_number) do
    fname = [
      "day",
      if(ctx.day < 10, do: "0"),
      ctx.day,
      if(part_number, do: "_#{part_number}"),
      if(ctx.test, do: "_test"),
      ".txt"
    ]

    ~s{../data/y#{ctx.year}/#{Enum.join(fname)}}
  end

  @doc """
  Returns the contents of a data file as an array of lines with line
  endings stripped. File is found using year, day, part number and
  the testing flag.

  If the file is not found and +part_number+ > 1, try with part number 1.
  If that is not found, try it without a part number at all.

  If there is no data file, returns nil.
  """
  def read_data_file(ctx) do
    path = data_file_path(ctx, ctx.part)
    path = if !File.exists?(path) && ctx.part > 1, do: data_file_path(ctx, 1), else: path
    path = if !File.exists?(path), do: data_file_path(ctx, nil), else: path

    if !File.exists?(path) do
      raise "error: data file not found for #{inspect(ctx)}"
    end

    input_lines(path)
  end

  @doc """
  Returns non-empty lines from the data file for year, day, and part_number.
  If there is no data file, returns nil.

  Normally, empty lines are skipped but if `skip_empty_lines` is false
  then they're returned as well.
  """
  def data_lines(ctx, skip_empty_lines \\ true) do
    lines = read_data_file(ctx)

    if lines == nil do
      nil
    else
      lines |> Enum.reject(&(skip_empty_lines and &1 == ""))
    end
  end

  def input_lines(file) do
    file
    |> File.read!()
    |> String.trim_trailing()
    |> String.split("\n")
  end
end
