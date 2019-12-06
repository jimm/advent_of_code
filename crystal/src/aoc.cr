#!/usr/bin/env crystal

require "option_parser"
require "./day"
require "./y2019/*"

# AoC is the driver for running Advent of Code solutions written in Crystal.
class AoC
  VERSION = "0.1.0"

  @@registry = {} of String => Day.class

  def self.register(klazz : Class)
    year_day = /Year(\d+)::Day(\d+)/
    year_day.match(klazz.name)
    @@registry["#{$1}.#{$2.to_i}"] = klazz.as(Day.class)
  end

  def self.run
    now = Time.now
    year = now.year
    day = now.day
    part_number = 1
    testing = false

    OptionParser.parse! do |parser|
      parser.banner = "usage: aoc [options] part_number"
      parser.on("-y YEAR", "--year YEAR", "Year (default is current year)") { |val| year = val }
      parser.on("-d DAY", "--day DAY", "Day (default is current day of the month)") { |val| day = val }
      parser.on("-t", "--test", "Turns on test flag") { testing = true }
    end
    part_number = ARGV[0].to_i

    klazz = @@registry["#{year}.#{day}"]
    if klazz
      klazz.new(part_number, testing).run
    else
      puts("error: year #{year} day #{day} not found")
    end
  end
end

AoC.run
