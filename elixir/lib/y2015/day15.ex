# Science for Hungry People

defmodule Y2015.Day15 do
  use Common.File

  @input_file default_input_path()
  @parse_regex ~r{(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)}

  defstruct [:name, :capacity, :durability, :flavor, :texture, :calories]

  # max attributes
  def run1 do
    ingredients = read_ingredients()

    all_possible_measures(length(ingredients), 100)
    |> Enum.map(&scores(ingredients, &1))
    |> Enum.map(fn score -> score |> tl |> Enum.reduce(1, &*/2) end)
    |> Enum.max()
  end

  # best 500 calorie
  def run2 do
    ingredients = read_ingredients()

    all_possible_measures(length(ingredients), 100)
    |> Enum.map(&scores(ingredients, &1))
    |> Enum.filter(fn [calories | _] -> calories == 500 end)
    |> Enum.map(fn score -> score |> tl |> Enum.reduce(1, &*/2) end)
    |> Enum.max()
  end

  defp read_ingredients do
    File.stream!(@input_file)
    |> Enum.reduce([], fn line, acc ->
      [name, capacity, durability, flavor, texture, calories] = parse(line)

      [
        %Y2015.Day15{
          name: name,
          capacity: String.to_integer(capacity),
          durability: String.to_integer(durability),
          flavor: String.to_integer(flavor),
          texture: String.to_integer(texture),
          calories: String.to_integer(calories)
        }
        | acc
      ]
    end)
    |> Enum.reverse()
  end

  defp parse(line) do
    Regex.run(@parse_regex, line) |> tl
  end

  # defp all_possible_scores(ingredients, f) do
  #   all_possible_measures(length(ingredients), 100)
  #   |> Enum.map(&(f.(ingredients, &1)))
  # end

  defp all_possible_measures(1, total), do: [[total]]

  defp all_possible_measures(n, total) do
    for first <- 0..total,
        rest <- all_possible_measures(n - 1, total - first) do
      [first | rest]
    end
  end

  defp scores(ingredients, measures) do
    ingredients
    |> Enum.zip(measures)
    |> Enum.map(fn {ingredient, measure} ->
      [
        measure * ingredient.calories,
        measure * ingredient.capacity,
        measure * ingredient.durability,
        measure * ingredient.flavor,
        measure * ingredient.texture
      ]
    end)
    |> Enum.reduce([0, 0, 0, 0, 0], fn [cal, c, d, f, t], [acal, ac, ad, af, at] ->
      [acal + cal, ac + c, ad + d, af + f, at + t]
    end)
    |> Enum.map(fn
      val when val < 0 -> 0
      val -> val
    end)
  end
end
