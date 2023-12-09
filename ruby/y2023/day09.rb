#!/usr/bin/env ruby
#
# Mirage Maintenance

require_relative '../day'

class Day09 < Day
  def do_part1(lines)
    lines.map do |line|
      next_value(line.split.map(&:to_i))
    end.sum
  end

  def do_part2(lines)
    lines.map do |line|
      prev_value(line.split.map(&:to_i))
    end.sum
  end

  private

  def next_value(vals)
    seqs = gen_diff_seqs(vals)
    (seqs.length - 2).downto(0) do |i|
      seqs[i] << seqs[i][-1] + seqs[i + 1][-1]
    end
    seqs[0][-1]
  end

  def prev_value(vals)
    seqs = gen_diff_seqs(vals)
    (seqs.length - 2).downto(0) do |i|
      seqs[i].unshift(seqs[i][0] - seqs[i + 1][0])
    end
    seqs[0][0]
  end

  def gen_diff_seqs(vals)
    seqs = [vals]
    seqs << diffs(seqs[-1]) until seqs[-1].all?(&:zero?)
    seqs[-1] << 0
    seqs
  end

  def diffs(vals)
    vals.each_cons(2).map { |a, b| b - a }
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
