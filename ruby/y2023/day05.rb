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

  def do_part1(lines)
    data = parse(lines, false)
    find_min_location_array(data)
  end

  def do_part2(lines)
    data = parse(lines, true)
    warn "data[:seeds] = #{data[:seeds]}" # DEBUG
    return 42 unless @testing             # DEBUG

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
    data[:seeds].map do |seed_range|
      seed_range.map do |seed_num|
        MAP_ORDER.inject(seed_num) do |n, map_sym|
          range_mappings = data[map_sym]
          n = lookup(n, range_mappings)
        end
      end.min
    end.min
  end

  def lookup(n, range_mappings)
    useable_range = range_mappings.detect do |rm|
      rm[:src_range].include?(n)
    end
    return n unless useable_range

    n + (useable_range[:dest_range].min - useable_range[:src_range].min)
  end

  def parse(lines, seeds_are_ranges)
    data = {}
    current_map_data = []
    lines.each do |line|
      case line
      when /^seeds/
        data[:seeds] = []
        nums = line[7..].split(' ').map(&:to_i)
        if seeds_are_ranges # part 2
          nums.each_slice(2) do |start, len|
            data[:seeds] << (start...(start + len))
          end
          data[:seeds] = merge_ranges(data[:seeds])
        else
          data[:seeds] = nums
        end
      when /([-\w]+) map:/
        current_map_data = []
        data[::Regexp.last_match(1).to_sym] = current_map_data
      else
        nums = line.split(' ').map(&:to_i)
        current_map_data << {
          dest_range: (nums[0]...(nums[0] + nums[2])),
          src_range: (nums[1]...(nums[1] + nums[2]))
        }
      end
    end
    data
  end

  # Reduces +ranges+ to the min number of ranges by eliminating overlaps.
  # Modifies +ranges+ in-place.
  def merge_ranges(ranges)
    ranges.sort! { |r0, r1| r0.min <=> r1.min }
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
    ranges.each { |r| puts "#{'%010d' % r.min}\t#{'%010d' % r.max}" } # DEBUG
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
