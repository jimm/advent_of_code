defmodule Common.AoC do
  import Common.{Context, Data}

  def run do
    OptionParser.parse_head(System.argv(),
      strict: [
        year: :integer,
        day: :integer,
        test: :boolean,
        debug: :boolean
      ]
    )
    |> run
  end

  def run(options) do
    {args, [part_str], _} = options

    module =
      String.to_existing_atom(
        "Elixir.Y#{args[:year]}.Day#{if(args[:day] < 10, do: "0")}#{args[:day]}"
      )

    ctx = %Common.Context{
      year: args[:year],
      day: args[:day],
      part: String.to_integer(part_str),
      module: module,
      test: args[:test],
      debug: args[:debug]
    }

    func =
      case ctx.part do
        1 -> &module.part1/2
        2 -> &module.part2/2
        _ -> throw("part number must be 1 or 2")
      end

    if ctx.test do
      run_test_chunks(ctx, func)
    else
      lines = data_lines(ctx)
      IO.puts(func.(ctx, lines))
    end
  end

  def run_test_chunks(ctx, func) do
    results =
      for chunk <- read_test_chunks(ctx) do
        run_chunk_test(ctx, chunk, func)
      end

    IO.puts("")

    if Enum.all?(results, &(&1 == :ok)) do
      IO.puts("ok")
    else
      results
      |> Enum.each(fn {expected, answer} ->
        IO.puts(["expected ", expected, ", got ", answer])
      end)

      IO.puts("fail")
    end
  end

  # Many times test data files have multiple tests. (These are usually files
  # that I've created based on multiple test cases provided by the problem
  # description and/or my needs.) The first line will start with any
  # character (for example, '#' or ';', but any character will do) and the
  # remainder of the lines is a comma-separated list of expected values for
  # the first part and optionally the second part.. The data/input for the
  # test is the following lines up to the next delimiter or EOF.
  #
  # This method returns a list of two-element lists where the first element
  # is the expected line, minus the delimter and any leading whitespace, and
  # the second element is the array of strings contains the data lines for
  # that test.
  def read_test_chunks(ctx) do
    data = data_lines(ctx, false)
    expected_line_char = data |> hd |> :binary.at(0)
    chunkify(data, expected_line_char, nil, [])
  end

  @doc """
  Breaks up data lines into {expected, [lines]} chunks
  """
  def chunkify([], _, curr_chunk, chunks) do
    # Both the chunks and the lines within them are reversed. Undo that.
    [curr_chunk | chunks]
    |> Enum.reverse()
    |> Enum.map(fn {expected_vals, lines} -> {expected_vals, Enum.reverse(lines)} end)
  end

  def chunkify([line | rest], expected_line_char, curr_chunk, chunks) do
    {new_curr_chunk, new_chunks} =
      if line != "" and :binary.at(line, 0) == expected_line_char do
        # expected values line, start of a new chunk
        expected_values =
          line
          |> String.slice(1..(String.length(line) - 1))
          |> String.trim()
          |> String.split(",")

        new_chunks = if curr_chunk == nil, do: [], else: [curr_chunk | chunks]
        {{expected_values, []}, new_chunks}
      else
        # a data line
        {expected_vals, curr_lines} = curr_chunk
        {{expected_vals, [line | curr_lines]}, chunks}
      end

    chunkify(rest, expected_line_char, new_curr_chunk, new_chunks)
  end

  # Returns :ok if all is well, else returns an {expected, answer} tuple.
  def run_chunk_test(ctx, {expected_values, data_lines}, func) do
    expected = Enum.at(expected_values, ctx.part - 1)
    answer = "#{func.(ctx, data_lines)}"

    if expected == answer do
      IO.write(".")
      :ok
    else
      IO.write("f")
      {expected, answer}
    end
  end
end
