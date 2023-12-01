#!/usr/bin/env ruby
#
# Trebuchet?!

require_relative '../day'

class Day01 < Day
  def do_part1(lines)
    lines.sum do |line|
      digits = line.gsub(/[^\d]+/, '')
      (digits[0] + digits[-1]).to_i
    end
  end

  def do_part2(lines)
    lines.sum do |line|
      line = words_to_digits(line)
      digits = line.gsub(/[^\d]+/, '')
      (digits[0] + digits[-1]).to_i
    end
  end

  def words_to_digits(str)
    out = ''
    idx = 0
    while idx < str.length
      found = false
      %w[zero one two three four five six seven eight nine].each_with_index do |s, i|
        next unless str[idx, s.length] == s

        out << i.to_s
        found = true
        break
      end
      out << str[idx] unless found
      idx += 1
    end
    out
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2023, 1)
end
