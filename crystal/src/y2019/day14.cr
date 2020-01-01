require "../day"

module Year2019
  # The list rules of are modeled as a tree. Root node is the FUEL formula.
  # The tree is built out of a Hash where keys are {amount, chemical} and
  # values are arrays of the same.
  #
  # Terminology: everything but FUEL and ORE is an "ingredient". An
  # "element" is an ingredient that can be produced directly from ORE. Any
  # other ingredient is an "alloy".
  class Day14 < Day
    def part1
      if @testing
        puts("ok") if run_test1()
      else
        puts("not yet implemented")
      end
    end

    def part2
      puts("not yet implemented")
    end

    def parse_rules(rules)
      parsed = {} of {Int32, String} => Array({Int32, String})
      rules.each do |rule|
        inputs_str, output_str = rule.split(" => ")
        inputs = inputs_str.split(", ").map do |input|
          num_str, chemical = input.split(" ")
          {num_str.to_i, chemical}
        end
        num_str, chemical = output_str.split(" ")
        output = {num_str.to_i, chemical}
        parsed[chemical] = {num_str.to_i, inputs}
      end
      parsed
    end

    def calc_ore_needed(formulas)
      puts("formulas #{formulas}") # DEBUG
      elements_needed = calc_ingredients_needed(formulas, formulas["FUEL"])
    end

    # Returns a list of {amount, element} need to create FUEL. *satisfy*
    # should start with the inputs to FUEL.
    def calc_elements_needed(formulas, satisfy)
      puts("calc_elements_needed satisfy #{satisfy}") # DEBUG
      if satisfy.all? { |rule| rule[1] == "ORE" }
        return satisfy.sum_by { |sat| sat[0] }
      end
      new_satisfy = satisfy.flat_map do |sat|
        if sat[1] == "ORE"
          [sat]
        else
          # FIXME
        end
      end
      calc_elements_needed(formulas, new_satisfy)
    end

    # Recursively finds elements and adds counts to *needed*.
    def calc_ingredients_needed(formulas, satisfy, needed)
      return needed if satisfy.empty?
      goal = satisfy[0]
      formula_for_goal = formulas[goal]
      inputs = formula_for_goal[1]
      if inputs.size == 1 && inputs[0][1] == "ORE"
        needed << goal[0]
      else
        satisfy << formulas
      end
      inputs_to_goal = formulas[goal]
      calc_ore_needed(formulas, satisfy[1..], new_needed)
    end

    def run_test1
      ok = true
      data_chunks(data_lines())
        .each do |data_chunk|
          expected_line, rules_lines = data_chunk
          expected = expected_line.to_i
          formulas = parse_rules(rules_lines)
          puts(formulas) # DEBUG
          # result = run_test1(formulas, expected)
          # ok &&= result
        end
      ok
    end

    def run_test1(formulas, expected)
      needed = calc_ore_needed()
      if needed != expected
        puts("expected #{expected}, found #{needed}")
        return false
      end
      true
    end
  end
end

AoC.register(Year2019::Day14)
