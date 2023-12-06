#!/usr/bin/env ruby
#
# If You Give A Seed A Fertilizer

require 'set'
require_relative '../day'

class Day05 < Day
  MAP_ORDER = %i[
    seed-to-soil soil-to-fertilizer fertilizer-to-water water-to-light light-to-temperature
    temperature-to-humidity humidity-to-location
  ]
  MAP_BEGIN = 0
  MAP_END = 1
  MAP_DELTA = 2

  def do_part1(lines)
    data = parse(lines, false)
    find_min_location_array(data)
  end

  def do_part2(lines)
    data = parse(lines, true)
    find_min_location_ranges(data)
  end

  private

  def find_min_location_array(data)
    data[:seeds].map do |seed_num|
      MAP_ORDER.inject(seed_num) do |n, map_sym|
        range_mappings = data[map_sym]
        n = lookup(n, range_mappings)
      end
    end.min
  end

  def find_min_location_ranges(data)
    # The idea: take each seed range and pass it through all of the
    # mappings, splitting it up into new ranges as needed. After that's
    # done, take the minimum of the resulting ranges and that's the min
    # location for that seed range. Finally, take the min of all of those
    # seed range passes.

    range_lists = data[:seeds].map do |seed_range|
      MAP_ORDER.inject([seed_range]) do |rs, map_sym|
        range_mappings = data[map_sym]
        new_seed_ranges(rs, range_mappings)
      end
    end
    range_lists.flatten.map(&:min).min
  end

  # We know that the range mappings are sorted and do not overlap, so we can
  # bail early.
  def lookup(n, range_mappings)
    range_mappings.each_with_index do |rm, i|
      return n + rm[MAP_DELTA] if n >= rm[MAP_BEGIN] && n <= rm[MAP_END]
    end
    n
  end

  # Takes a list of +ranges+ of input values and returns a list of sorted
  # Ranges that are the result of passing them through +range_mappings+. For
  # example with input ranges +[Range(1..7)]+ and range mappings +[[2, 4,
  # 10]]+ this will return +[Range(1..1), Range(5..7), Range(12..14)]+.
  def new_seed_ranges(ranges, range_mappings)
    mapped = []
    range_mappings.each do |rm|
      needs_mapping = []
      ranges.each do |r|
        if r.max < rm[MAP_BEGIN] || r.min > rm[MAP_END] # entirely outside
          needs_mapping << r
        elsif r.min >= rm[MAP_BEGIN] && r.max <= rm[MAP_END] # entirely inside
          mapped << (r.min + rm[MAP_DELTA]..r.max + rm[MAP_DELTA])
          break
        else
          needs_mapping << (r.min...rm[MAP_BEGIN]) if r.min < rm[MAP_BEGIN] # before range mapping
          needs_mapping << (rm[MAP_END] + 1..r.max) if r.max > rm[MAP_END]  # after range mapping
          # inside range mapping
          mapped << ([r.min, rm[MAP_BEGIN]].max + rm[MAP_DELTA]..[r.max, rm[MAP_END]].min + rm[MAP_DELTA])
        end
      end

      ranges = merge_sorted_ranges(needs_mapping.sort_by(&:min).uniq)
    end
    (ranges + mapped).sort_by(&:min).uniq
  end

  # Returns a dict with :seeds which is a list of ranges and with each
  # mapping expressed as an array in the form [source_begin, source_end,
  # delta_to_apply]. Both seed ranges and mappings will be sorted by their
  # start values.
  def parse(lines, seeds_are_ranges)
    data = {}
    current_map_data = nil
    lines.each do |line|
      case line
      when /^seeds/
        data[:seeds] = []
        nums = line[7..].split(' ').map(&:to_i)
        if seeds_are_ranges # part 2
          nums.each_slice(2) do |start, len|
            data[:seeds] << (start...(start + len))
          end
          data[:seeds].sort_by!(&:min)
          data[:seeds] = merge_sorted_ranges(data[:seeds])
        else
          data[:seeds] = nums
        end
      when /([-\w]+) map:/
        current_map_data.sort_by!(&:first) if current_map_data
        current_map_data = []
        data[::Regexp.last_match(1).to_sym] = current_map_data
      else
        nums = line.split(' ').map(&:to_i)
        # [source_begin, source_end, delta]
        current_map_data << [nums[1], nums[1] + nums[2] - 1, nums[0] - nums[1]]
      end
      current_map_data.sort_by!(&:first) if current_map_data
    end
    data
  end

  # Reduces +ranges+ to the min number of ranges by eliminating overlaps.
  # Modifies +ranges+ in-place.
  def merge_sorted_ranges(ranges)
    offset = 0
    while offset < ranges.length - 1
      r0 = ranges[offset]
      r1 = ranges[offset + 1]
      if r0.max >= r1.min
        ranges[offset, 2] = (r0.min..[r0.max, r1.max].max)
      else
        offset += 1
      end
    end
    ranges
  end

  def debug_print_ranges(ranges)
    ranges.each { |r| puts "#{'%010d' % r.min}\t#{'%010d' % r.max}" }
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
