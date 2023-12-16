#!/usr/bin/env ruby
#
# usage: aoc.rb [-y|--year=year] [-d|--day-day] [--D|--DEBUG] [-t|--test] 1|2

require 'optparse'
require_relative 'day'

# If we pass in a file name, we parse the year and day of the month from its
# path. Else we start with the current year and day of the month. Both can
# be overridden by command line arguments.
def aoc(file = nil)
  if file
    day = File.basename(file)[3, 2].to_i
    year = File.basename(File.dirname(file))[1..].to_i
  else
    now = Time.now
    day = now.day
    year = now.year
  end
  testing = false

  OptionParser.new do |opts|
    opts.on('-y YEAR', '--year YEAR', 'year') { |arg| year = arg.to_i }
    opts.on('-d DAY', '--day DAY', 'day') { |arg| day = arg.to_i }
    opts.on('-t', '--testing', 'testing') { |_| testing = true }
    opts.on('-D', '--debug', 'debug') { |_| $DEBUG = true }
  end.parse!
  if ARGV[0].nil?
    warn 'error: must specify part number 1 or 2'
    exit(1)
  end
  part_number = ARGV[0].to_i
  day_str = '%02d' % day

  klass_name = "Day#{day_str}"
  require_relative "y#{year}/day#{day_str}.rb" unless Object.const_defined?(klass_name)
  Object.const_get(klass_name).new(year, day, part_number, testing).run
end

aoc if __FILE__ == $PROGRAM_NAME
