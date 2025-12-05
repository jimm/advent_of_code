#!/usr/bin/env ruby
#
# Cafeteria

require_relative '../day'

class Day05 < Day
  def do_part1(lines)
    ranges, ids = parse(lines)
    fresh = []
    ids.select do |id|
      fresh << id if ranges.detect { |r| r.include?(id) }
    end
    fresh.length
  end

  def do_part2(lines)
    test_merge_ranges if $DEBUG
    ranges, _ids = parse(lines)
    ranges = consolidate_ranges(ranges)
    ranges.map(&:size).sum
  end

  private

  # Keeps merging ranges until no more merging is possible
  def consolidate_ranges(ranges)
    consol = [ranges[0]]
    num_merges = 0
    ranges[1..].each do |range|
      new_consol = Set.new
      consol.each do |crange|
        merged = merge_ranges(range, crange)
        num_merges += 1 if merged.length == 1
        new_consol += merged
      end
      consol = new_consol.to_a
    end
    consol = consolidate_ranges(consol) if num_merges > 0
    consol
  end

  # Merges r1 and r2 if possible and returns an array with one or two
  # ranges.
  def merge_ranges(r1, r2)
    # Sort by beginning so we have fewer cases to handle
    r1, r2 = r2, r1 if r1.begin > r2.begin
    if r1.end < r2.begin - 1
      [r1, r2]
    else
      [(r1.begin..[r1.end, r2.end].max)]
    end
  end

  def parse(lines)
    ranges = []
    ids = []
    lines.each do |line|
      next if line.empty?

      a, b = line.split('-')
      if b.nil? # not a range
        ids << a.to_i
      else
        ranges << (a.to_i..b.to_i)
      end
    end
    [ranges, ids]
  end

  # ==== testing

  def test_merge_ranges
    [
      [(1..10), (1..10), [(1..10)]], # same
      [(3..8), (1..10),  [(1..10)]],  # r1 inside r2, no same begin/end
      [(1..8), (1..10),  [(1..10)]],  # r1 inside r2, same begin
      [(3..10), (1..10), [(1..10)]], # r1 inside r2, same end
      [(1..10), (3..8), [(1..10)]],  # r1 outside r2, no same begin/end
      [(1..10), (1..8), [(1..10)]],  # r1 outside r2, same begin
      [(1..10), (3..10), [(1..10)]], # r1 outside r2, same end
      [(0..8), (1..10), [(0..10)]],  # r1 left of r2 with overlap, diff end
      [(0..10), (1..10), [(0..10)]], # r1 left of r2 with overlap, same end
      [(3..11), (1..10), [(1..11)]], # r1 right of r2 with overlap, diff begin
      [(1..11), (1..10), [(1..11)]], # r1 right of r2 with overlap, same begin
      [(1..10), (15..20), [(1..10), (15..20)]], # no overlap, r1 = L r2 = R
      [(15..20), (1..10), [(1..10), (15..20)]], # no overlap, r1 = R r2 = L
      [(1..10), (11..20), [(1..20)]], # r1 end == r2.begin - 1
      [(11..20), (1..10), [(1..20)]]  # r2 end == r1.begin - 1
    ].each { |args| merge_test(*args) }
    puts 'test_merge_ranges ok'
  end

  def merge_test(r1, r2, expected)
    merged = merge_ranges(r1, r2)
    return unless merged != expected

    raise "\nmerge_ranges(#{r1}, #{r2}) expected #{expected}, got #{merged}"
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
