#!/usr/bin/env ruby
#
# usage: aoc.rb [--year=year] [--day-day] [--test] 1|2

require 'optparse'
require_relative 'day'

if __FILE__ == $PROGRAM_NAME
  now = Time.now
  year = now.year
  day = now.day
  testing = false

  OptionParser.new do |opts|
    opts.on("-y YEAR", "--year YEAR", "year") { |arg| year = arg.to_i }
    opts.on("-d DAY", "--day DAY", "day") { |arg| day = arg.to_i }
    opts.on("-t", "--testing", "testing") { |_| testing = true }
  end.parse!
  if ARGV[0].nil?
    $stderr.puts "error: must specify part number 1 or 2"
    exit(1)
  end
  part_number = ARGV[0].to_i
  day_str = '%02x' % day

  require_relative "y#{year}/day#{day_str}.rb"
  Object.const_get("Day#{day_str}").new(year, day, part_number, testing).run
end
