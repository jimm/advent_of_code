# ================ debugging helpers ================

class Object
  # Prints self.to_s to stdout and returns self, so that this method can be
  # chained.
  def pdebug(msg = nil)
    puts("#{msg}#{msg ? ' ' : ''}#{self}") if $DEBUG
    self
  end

  # Prints self.inspect to stdout and returns self, so that this method can
  # be chained.
  def idebug(msg = nil)
    puts("#{msg}#{msg ? ' ' : ''}#{inspect}") if $DEBUG
    self
  end
end

def debug(msg)
  puts(msg) if $DEBUG
end

def clear_screen
  print("\e[0;0H\e[J")
end

# ================

# A framework for running tests and solutions that also includes methods for
# reading data and running tests.
class Day
  attr_reader :year, :day, :part_number, :testing

  def initialize(year, day, part_number, testing)
    @year = year
    @day = day
    @part_number = part_number
    @testing = testing
  end

  # ================ running solutions ================

  def run
    fname = "part#{@part_number}"
    fname += '_tests' if @testing
    send(fname.to_sym)
  end

  # Reads the data if +lines+ is +nil+, calls +do_part1+, and prints the
  # returned value. Passes the lines in to #do_part1. If there is no data
  # file, then +lines+ will be +nil+.
  def part1(lines = nil)
    lines ||= data_lines(1)
    puts do_part1(lines)
  end

  # This method, run by both +part1+ and +part1_tests+, calculates the
  # answer and returns it. When run by +part1_tests+, it is called once per
  # test. If there is no part 1 data file then +lines+ will be +nil+.
  def do_part1(lines)
    raise 'subclasses must implement'
  end

  # Runs the tests for part 1. Will call `do_part1` once for each test.
  def part1_tests
    do_tests(1)
  end

  # Reads the data if +lines+ is +nil+, calls +do_part2+, and prints the
  # returned value. Passes the lines in to #do_part2. If there is no data
  # file, then +lines+ will be +nil+.
  def part2
    lines ||= data_lines(2)
    puts do_part2(lines)
  end

  # This method, run by both +part2+ and +part2_tests+, calculates the
  # answer and returns it. When run by +part2_tests+, it is called once per
  # test. If there is no part 2 data file then +lines+ will be +nil+.
  def do_part2(lines)
    raise 'subclasses must implement'
  end

  # Runs the tests for part 2. Will call `do_part2` once for each test.
  def part2_tests
    do_tests(2)
  end

  # Given an expected answer and a block, yields the expected answer to the
  # block which must return the answer. Prints success or failure.
  #
  # Despite their names, +run_chunk_tests+ does not call this method.
  def run_one_test(expected)
    answer = yield(expected)
    if answer == expected
      puts('.')
      puts('ok')
    else
      puts('F')
      puts("error: expected #{expected}, got #{answer}")
    end
  end

  # Given an optional part number, reads each test chunk and yields the
  # expected value as a string and the data lines. The block must return a
  # [boolean, answer] pair or [boolean, answer, expected] triplet. Prints
  # success or failure for all the tests.
  #
  # A test chunk starts with a line starting with '# <expected>[,<expected>]'
  # (any beginning char will do, actually) and ends at the next such line or
  # the end of the file. The first expected value is for part 1, the second
  # for part 2.
  def run_chunk_tests(part_number = @part_number)
    # Look first for the test data file for the given part number. If
    # anything goes wrong and part_number == 2, try looking for the test
    # data file for part 1.
    chunks = begin
      test_chunks(part_number)
    rescue StandardError
      if part_number == 2
        begin
          test_chunks(1)
        rescue StandardError
          no_tests
        end
      else
        no_tests
      end
    end

    errors = []
    chunks.each_with_index do |chunk, test_run|
      expected, lines = chunk
      expected_list = expected.split(',')
      expected = part_number == 1 ? expected_list[0] : (expected_list[1] || expected_list[0])

      answer = yield(lines)
      if answer.to_s == expected.to_s
        print('.')
      else
        print('F')
        errors << [test_run + 1, expected, answer]
      end
    end
    puts
    if errors.empty?
      puts('ok')
    else
      errors.each { |err| puts("run #{err[0]}: expected #{err[1]}, got #{err[2]}") }
    end
  end

  # Default +do_tests+ implementation.
  def do_tests(part_number = @part_number)
    run_chunk_tests(part_number) { |lines| send("do_part#{part_number}".to_sym, lines) }
  end

  def no_tests
    return unless @testing

    puts('no tests available')
    exit(0)
  end

  # ================ reading data files ================

  private

  # Returns the path to the data file, using +part_number+. If +part_number+
  # is nil, the file name won't containthe part number.
  def data_file_path(part_number)
    fname = "day#{format('%02d', @day)}"
    fname += "_#{part_number}" if part_number
    fname += '_test' if @testing
    File.join(__dir__, "../data/y#{@year}", "#{fname}.txt")
  end

  public

  # Returns the contents of a data file as an array of lines with line
  # endings stripped. File is found using year, day, part number and
  # the testing flag.
  #
  # If the file is not found and +part_number+ > 1, try with part number 1.
  # If that is not found, try it without a part number at all.
  #
  # If there is no data file, returns nil.
  def read_data_file(part_number = @part_number)
    path = data_file_path(part_number)
    path = data_file_path(1) if !File.exist?(path) && part_number > 1
    path = data_file_path(nil) unless File.exist?(path)

    raise "error: data file not found for year = #{year}, day = #{day}, part = #{part}" unless File.exist?(path)

    IO.readlines(path).map(&:chomp!)
  end

  # Returns non-empty lines from the data file for @year, @day, and
  # part_number (default @part_number). If there is no data file, returns
  # nil.
  #
  # Normally, empty lines are skipped but if +skip_empty_lines+ is false
  # then they're returned as well.
  def data_lines(part_number = @part_number, skip_empty_lines = true)
    lines = read_data_file(part_number)
    return nil unless lines

    lines = lines[0..-2] if lines[-1].nil? # no newline at end of file
    lines.reject!(&:empty?) if skip_empty_lines
    lines
  end

  # Many times test data files have multiple tests. (These are usually files
  # that I've created based on multiple test cases provided by the problem
  # description and/or my needs.) The first line will start with any
  # character (for example, '#' or ';', but any character will do) and the
  # remainder of the lines is a comma-separated list of expected values for
  # the first part and optionally the second part.. The data/input for the
  # test is the following lines up to the next delimiter or EOF.
  #
  # This method returns a list of two-element lists where the first element
  # is the expected line, minus the delimter and any leading whitespace, and
  # the second element is the array of strings contains the data lines for
  # that test.
  def test_chunks(part_number = @part_number)
    chunks = []
    chunk_index = -1
    expected_line_char = nil
    first_line = true
    data_lines(part_number, false).each do |line|
      if first_line
        expected_line_char = line[0]
        first_line = false
      end
      if line[0] == expected_line_char
        # remove last empty line of previous chunk
        chunks[chunk_index][1].pop if chunk_index >= 0 && chunks[chunk_index][1].last.empty?

        chunk_index += 1
        chunks[chunk_index] = [line[1..-1].strip, []]
      elsif chunk_index >= 0
        chunks[chunk_index][1] << line
      end
    end
    chunks.pop if chunks.last.empty?
    chunks
  end
end
