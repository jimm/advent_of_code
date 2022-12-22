#!/usr/bin/env ruby
#
# Monkey Math

require_relative '../day'

class Day21 < Day
  def part1
    puts do_part1(data_lines(1))
  end

  def part1_tests
    do_tests
  end

  def part2
    puts do_part2(data_lines(1))
  end

  def part2_tests
    do_tests
  end

  private

  def do_tests
    run_chunk_tests(1) do |expected, lines|
      expected = expected.split(',')[@part_number - 1].to_i
      answer = send(:"do_part#{@part_number}", lines)
      [answer == expected, answer, expected]
    end
  end

  def do_part1(lines)
    ast = parse(lines)
    while perform_substitutions(ast)
      # nop
    end
    eval_ast(ast, ast[:root])
  end

  def do_part2(lines)
    ast = parse(lines)
    ast.delete(:humn) # or some special value?
    ast[:root][1] = :<=>
    while perform_substitutions(ast)
      # nop
    end
    ast[:humn] = 5
    val = eval_ast(ast, ast[:root])
    offset = val > 0 ? -1 : 1
    # FIXME: taking too long
    while val != 0
      ast[:humn] += offset
      val = eval_ast(ast, ast[:root])
    end
    ast[:humn]
  end

  def perform_substitutions(ast)
    changed = false
    ast.keys.each do |key|
      next if ast[key].is_a?(Numeric)

      left, op, right = *ast[key]
      if left.instance_of?(Symbol) && ast[left].is_a?(Numeric)
        left = ast[left]
        changed = true
      end
      if right.instance_of?(Symbol) && ast[right].is_a?(Numeric)
        right = ast[right]
        changed = true
      end
      ast[key] = if left.is_a?(Numeric) && right.is_a?(Numeric)
                   changed = true
                   eval("#{left} #{op} #{right}")
                   val = eval("#{left} #{op} #{right}")
                 else
                   [left, op, right]
                 end
    end
    changed
  end

  def eval_ast(ast, op_or_num)
    return op_or_num if op_or_num.is_a?(Numeric)

    left_var, op, right_var = *op_or_num
    left = left_var.is_a?(Numeric) ? left_var : eval_ast(ast, ast[left_var])
    # this optimization is wrong for part2
    # ast[left_var] = left
    right = right_var.is_a?(Numeric) ? right_var : eval_ast(ast, ast[right_var])
    # this optimization is wrong for part2
    # ast[right_var] = right
    eval("#{left} #{op} #{right}")
  end

  def parse(lines)
    ast = {}
    lines.each do |line|
      line =~ /(\w+): (.*)/
      name = ::Regexp.last_match(1).to_sym
      case ::Regexp.last_match(2)
      when /(\d+)/
        ast[name] = ::Regexp.last_match(1).to_i
      when /(\w+) (.) (\w+)/
        ast[name] = [::Regexp.last_match(1).to_sym, ::Regexp.last_match(2).to_sym, ::Regexp.last_match(3).to_sym]
      end
    end
    ast
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2022, 21)
end
