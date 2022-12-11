#!/usr/bin/env ruby
#
# Transparent Origami

require 'set'
require_relative '../day'

class Day13 < Day
  def part1
    dots, folds = read_data(1)
    dots = fold(dots, folds[0])
    puts dots.size
  end

  def part2
    dots, folds = read_data(1)
    dots = folds.reduce(dots) { |ds, f| fold(ds, f) }
    print_dots(dots)
  end

  def read_data(part_num)
    dots = Set.new
    folds = []
    data_lines(part_num).each do |line|
      if line.start_with?('fold along ')
        line =~ /(x|y)=(\d+)/
        folds << [Regexp.last_match(1).to_sym, Regexp.last_match(2).to_i]
      else
        x, y = line.split(',').map(&:to_i)
        dots.add([x, y])
      end
    end
    [dots, folds]
  end

  def fold(dots, fold)
    sym = fold[0]
    fold_at = fold[1]
    base = fold_at * 2
    new_dots = Set.new
    # we've been guaranteed that no dots lie on the fold line
    dots.each do |dot|
      if sym == :x && dot[0] > fold_at
        new_dots.add([base - dot[0], dot[1]])
      elsif sym == :y && dot[1] > fold_at
        new_dots.add([dot[0], base - dot[1]])
      else
        new_dots.add(dot)
      end
    end
    new_dots
  end

  def print_dots(dots)
    rows = dots.group_by { |d| d[1] }
    rows.keys.sort.each do |y|
      cols = rows[y].map(&:first)
      0.upto(cols.max) do |i|
        print cols.include?(i) ? '#' : ' '
      end
      puts
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc
end
