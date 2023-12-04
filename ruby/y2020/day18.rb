#!/usr/bin/env ruby
#
# Operation Order

require_relative '../day'

class Day18 < Day
  def part1
    do_part(:next_op_index_equal_precedence)
  end

  def part1_tests
    do_tests(:next_op_index_equal_precedence)
  end

  def part2
    do_part(:next_op_mult_high_precedence)
  end

  def part2_tests
    do_tests(:next_op_mult_high_precedence)
  end

  def do_part(next_op_index_func)
    total =
      data_lines(1)
      .map { |line| new_math_eval(parse(line), next_op_index_func) }
      .sum
    puts(total)
  end

  def do_tests(next_op_index_func)
    run_chunk_tests do |expected, lines|
      answer = new_math_eval(parse(lines[0]), next_op_index_func)
      [answer == expected.to_i, answer]
    end
  end

  def new_math_eval(expr, next_op_index_func)
    return expr if expr.is_a?(Integer)
    return new_math_eval(expr[0], next_op_index_func) if expr.length == 1

    op_index = send(next_op_index_func, expr)
    left_val = new_math_eval(expr[op_index - 1], next_op_index_func)
    op = expr[op_index]
    right_val = new_math_eval(expr[op_index + 1], next_op_index_func)
    expr[op_index - 1, 3] = left_val.send(op, right_val)
    new_math_eval(expr, next_op_index_func)
  end

  def next_op_index_equal_precedence(expr)
    i1 = expr.index(:+)
    i2 = expr.index(:*)
    return i2 if i1.nil?
    return i1 if i2.nil?
    return i1 if i1 < i2

    i2
  end

  def next_op_mult_high_precedence(expr)
    i = expr.index(:+)
    return i if i

    expr.index(:*)
  end

  def parse(line)
    array_innards = line
                    .gsub(/\(/, '[')
                    .gsub(/\)/, ']')
                    .gsub(/([+*])/, ':\1')
                    .gsub(/ /, ',')
    eval('[' + array_innards + ']')
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
