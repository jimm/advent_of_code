#!/usr/bin/env ruby
#
# Parabolic Reflector Dish

require_relative '../day'
require_relative '../map'

class Day14 < Day
  NUM_CYCLES = 1_000_000_000

  def do_part1(lines)
    m = Map.new(lines)
    tilt_north(m)
    total_load(m)
  end

  def do_part2(lines)
    m = Map.new(lines)
    tilt_north(m)
    # tilt_north(m)
    # tilt_north(m)
    # tilt_north(m)
    total_load(m)
  end

  private

  def tilt_north(m)
    (1...m.height).each do |ri|
      (0...m.width).each do |ci|
        next unless m.at(ri, ci) == 'O'

        r_new = roll_north(m, ri, ci)
        next unless r_new

        m.set(r_new, ci, 'O')
        m.set(ri, ci, '.')
      end
    end
  end

  def roll_north(m, ri, ci)
    r_new = ri
    r_new -= 1 while r_new > 0 && m.at(r_new - 1, ci) == '.'
    r_new >= 0 && m.at(r_new, ci) == '.' ? r_new : nil
  end

  def total_load(m)
    score = 0
    m.each do |ri, ci, ch|
      score += (m.height - ri) if ch == 'O'
    end
    score
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
