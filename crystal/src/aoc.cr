#!/usr/bin/env crystal

require "option_parser"

# AoC is the driver for running Advent of Code solutions written in Crystal.
class AoC
  VERSION = "0.1.0"

  @@registry : Hash(String, Proc(Int32, Bool, Nil)) = {} of String => Proc(Int32, Bool, Nil)

  def self.register(key : String, proc : Proc(Int32, Bool, Nil))
    @@registry[key] = proc
  end

  def self.run
    now = Time.now
    year = now.year
    day = now.day
    part_number = 1
    test = false

    OptionParser.parse! do |parser|
      parser.banner = "usage: aoc [options] part_number"
      parser.on("-y YEAR", "--year YEAR", "Year (default is current year)") { |val| year = val }
      parser.on("-d DAY", "--day DAY", "Day (default is current day of the month)") { |val| day = val }
      parser.on("-t", "--test", "Turns on test flag") { test = true }
    end
    part_number = ARGV[0].to_i

    proc = @@registry["#{year}.#{day}"]
    if proc
      proc.call(part_number, test)
    else
      puts("error: year #{year} day #{day} not found")
    end
  end
end

# testing, testing
proc = ->(part_number : Int32, test : Bool) do
  puts("hello, part_number = #{part_number}, test = #{test}")
end
AoC.register("2019.3", proc)

AoC.run
