#!/usr/bin/env ruby
#
# Parabolic Reflector Dish

require_relative '../day'
require_relative '../map'

class Day14 < Day
  NUM_CYCLES = 1_000_000_000

  class CacheableMap < Map
    attr_accessor :rows

    def copy_map(m)
      @rows = m.rows.map { _1.dup } # makes a deep copy
      @height = m.height
      @width = m.width
    end

    def hash
      @hashed_value ||= @rows.map(&:join).join.hash
    end

    def clone(freeze: nil)
      new_map = super
      new_map.instance_variable_set(:@hashed_value, nil)
      new_map
    end
  end

  def do_part1(lines)
    m = CacheableMap.new(lines)
    @cache = {}
    m = tilt(m, :north)
    total_load(m)
  end

  def do_part2(lines)
    m = CacheableMap.new(lines)
    @cache = {}
    history = []
    NUM_CYCLES.times do |i|
      m = tilt(m, :north)
      m = tilt(m, :west)
      m = tilt(m, :south)
      m = tilt(m, :east)
      if history.map(&:hash).include?(m.hash)
        last_idx = history.map(&:hash).index(m.hash)
        m = history[last_idx + (NUM_CYCLES - 1 - i) % (i - last_idx)]
        break
      end
      history << m
    end
    total_load(m)
  end

  private

  def tilt(m, dir)
    key = [m.hash, dir]
    return @cache[key] if @cache[key]

    m = m.clone
    send("tilt_#{dir}".to_sym, m)
    @cache[key] = m
    m
  end

  def tilt_north(m)
    (1...m.height).each do |ri|
      (0...m.width).each do |ci|
        next unless m.at(ri, ci) == 'O'

        r_new, c_new = roll_north(m, ri, ci)
        next unless r_new && c_new

        m.set(r_new, c_new, 'O')
        m.set(ri, ci, '.')
      end
    end
  end

  def tilt_south(m)
    m.rows.reverse!
    tilt_north(m)
    m.rows.reverse!
  end

  def tilt_west(m)
    (0...m.height).each do |ri|
      (1...m.width).each do |ci|
        next unless m.at(ri, ci) == 'O'

        r_new, c_new = roll_west(m, ri, ci)
        next unless r_new && c_new

        m.set(r_new, c_new, 'O')
        m.set(ri, ci, '.')
      end
    end
  end

  def tilt_east(m)
    m.rows.each do |row|
      row.reverse!
    end
    tilt_west(m)
    m.rows.each do |row|
      row.reverse!
    end
  end

  def roll_north(m, ri, ci)
    r_new = ri
    r_new -= 1 while r_new > 0 && m.at(r_new - 1, ci) == '.'
    r_new >= 0 && m.at(r_new, ci) == '.' ? [r_new, ci] : [nil, nil]
  end

  def roll_west(m, ri, ci)
    c_new = ci
    c_new -= 1 while c_new > 0 && m.at(ri, c_new - 1) == '.'
    c_new >= 0 && m.at(ri, c_new) == '.' ? [ri, c_new] : [nil, nil]
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
