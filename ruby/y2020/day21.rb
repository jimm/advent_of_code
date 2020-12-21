# Allergen Assessment

class Food
  attr_reader :ingredients, :allergens

  def initialize(line)
    line =~ /(.*) \(contains (.*)\)/
    ingredients, contains = $1, $2
    @ingredients = $1.split(' ')
    @allergens = $2.split(', ')
  end
end


class Day21 < Day
  def part1
    foods = data_lines(1).map { |line| Food.new(line) }
    ingredients_tally = foods.flat_map(&:ingredients).tally
    allergens = foods.flat_map(&:allergens).uniq
    ingredients = ingredients_tally.keys

    allergen_ingredients = {}
    allergens.each do |allergen|
      foods.each do |food|
        if food.allergens.include?(allergen)
          allergen_ingredients[allergen] ||= food.ingredients
          allergen_ingredients[allergen] &= food.ingredients
        end
      end
    end

    non_allergenic = ingredients - allergen_ingredients.values.flatten.uniq
    puts(non_allergenic.map { |ingredient| ingredients_tally[ingredient] }.sum)
  end

  def part2
    foods = data_lines(1).map { |line| Food.new(line) }
    ingredients_tally = foods.flat_map(&:ingredients).tally
    allergens = foods.flat_map(&:allergens).uniq
    ingredients = ingredients_tally.keys

    allergen_ingredients = {}
    allergens.each do |allergen|
      foods.each do |food|
        if food.allergens.include?(allergen)
          allergen_ingredients[allergen] ||= food.ingredients
          allergen_ingredients[allergen] &= food.ingredients
        end
      end
    end

    uniquify_allergen_ingredients!(allergen_ingredients)
    sorted_ingredients = allergen_ingredients.keys.sort
                           .map { |k| allergen_ingredients[k].first }
    puts(sorted_ingredients.join(","))
  end

  def part2_tests
    no_tests
  end

  def uniquify_allergen_ingredients!(h)
    while h.values.any? { |vals| vals.length > 1 }
      unique_vals = h.keys
                      .select { |k| h[k].length == 1 }
                      .map { |k| h[k].first }
      h.each_key do |k|
        if h[k].length > 1
          h[k] = h[k] - unique_vals
        end
      end
    end
  end
end
