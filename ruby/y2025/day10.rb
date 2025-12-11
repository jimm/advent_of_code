#!/usr/bin/env ruby
#
# Factory

require_relative '../day'

class Day10 < Day
  class Machine
    attr_accessor :lights, :wiring, :joltage

    def initialize(lights, wiring, joltage)
      @lights = lights          # goal state; array of bools, true == light on
      @wiring = wiring          # array of arrays of numbers
      @joltage = joltage        # array of numbers
    end

    def min_config_press_count
      (0..nil).each do |n|
        return n if configurable_with_n_presses?(n)
      end
      0 # will never reach
    end

    def min_joltage_press_count
      (0..nil).each do |n|
        return n if joltage_set_with_n_presses?(n)
        return 0 if $DEBUG && n >= 15
      end
      0 # will never reach
    end

    def to_s
      "Machine(l=#{@lights.inspect}, w=#{@wiring.inspect}, j=#{@joltage.inspect})"
    end

    # ================ configuration ================

    def configurable_with_n_presses?(n)
      on_indexes = (0...@lights.length).select { |i| @lights[i] }
      @wiring.combination(n).any? { |schematic| configures?(on_indexes, schematic) }
    end

    # Returns true of the button presses in `schematic` can result in the
    # same `on_indexes` as @lights.
    def configures?(on_indexes, schematic)
      on_after_presses_indexes = schematic.flatten.tally.select { |_, v| v.odd? }.keys
      on_indexes.length == on_after_presses_indexes.length && on_indexes == on_after_presses_indexes.sort
    end

    # ================ joltage settings ================

    def joltage_set_with_n_presses?(n)
      max_presses = @wiring.map do |buttons|
        buttons.map { |button| @joltage[button] }.min
      end
      @wiring.combination(n).any? { |schematic| sets_joltage?(max_schematic, max_presses) }
    end

    # Returns true of the button presses in `schematic` can result in our
    # desired @joltages.
    #
    # `max_presses` is an optimization: a pre-calculated map from wiring
    # (e.g. [3, 5]) to max number of times it can be pressed before one of
    # the joltages will be exceeded.
    def sets_joltage?(schematic, max_presses)
      after_joltages = schematic.flatten.tally

      # Find out max number of times each button can be pressed
      schematic.map { |buttons| max_presses(buttons) }

      # We can drop early if there is some desired joltage but no
      # corresponding button
      return false if after_joltages.keys.length != @joltage.length

      # FIXME: doesn't account for number of button presses; always presses each button once

      @joltage.each_with_index { |j, i| return false if after_joltages[i] != j }
      true
    end

    # ================ misc ================
  end

  def do_part1(lines)
    machines_from_lines(lines)
      .map(&:min_config_press_count)
      .sum
  end

  def do_part2(lines)
    machines_from_lines(lines)
      .map(&:min_joltage_press_count)
      .sum
  end

  def machines_from_lines(lines)
    lines.map do |line|
      lights = ''
      wiring = []
      joltage = []
      line.split(' ').each do |part|
        if part[0] == '['
          lights = part[1..-2].chars.map { |ch| ch == '#' }
        elsif part[0] == '('
          wiring << part[1..-2].split(',').map(&:to_i)
        elsif part[0] == '{'
          joltage << part[1..-2].split(',').map(&:to_i)
        end
      end
      Machine.new(lights, wiring, joltage)
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
