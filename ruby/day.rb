class Day
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

  def part1
    raise 'subclasses must implement'
  end

  def part1_tests
    part1
  end

  def part2
    raise 'subclasses must implement'
  end

  def part2_tests
    part2
  end

  # Given an expected answer and a block, yields the expected answer to the
  # block, which must return a [boolean, answer] pair. Prints success or
  # failure.
  #
  # Despite their names, `run_chunk_tests` does not call this method.
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
  # A test chunk starts with a line starting with '# <expected>' and ends at
  # the next such line or the end of the file.
  def run_chunk_tests(part_number = @part_number)
    errors = []
    test_chunks(part_number).each do |expected, lines|
      ok, answer, optional_expected = yield(expected, lines)
      if ok
        print('.')
      else
        print('F')
        errors << [optional_expected || expected, answer]
      end
    end
    puts
    if errors.empty?
      puts('ok')
    else
      errors.each { |err| puts("expected #{err[0]}, got #{err[1]}") }
    end
  end

  def no_tests
    return unless @testing

    puts('no tests available')
    exit(0)
  end

  # ================ reading data files ================

  private

  # Returns the path to the data file, using `part_number`. If `part_number`
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
  # If the file is not found and `part_number` > 1, try with part number 1.
  # If that is not found, try it without a part number at all.
  def read_data_file(part_number = @part_number)
    path = data_file_path(part_number)
    path = data_file_path(1) if !File.exist?(path) && part_number > 1
    path = data_file_path(nil) unless File.exist?(path)
    raise "No data file found for year #{@year} day #{@day}" unless File.exist?(path)

    IO.readlines(path).map(&:chomp!)
  end

  # Returns non-empty lines from the data file for @year, @day, and
  # part_number (default @part_number).
  #
  # Normally, empty lines are skipped but if `skip_empty_lines` is false
  # then they're returned as well.
  def data_lines(part_number = @part_number, skip_empty_lines = true)
    lines = read_data_file(part_number)
    lines = lines[0..-2] if lines[-1].nil? # no newline at end of file
    lines.reject!(&:empty?) if skip_empty_lines
    lines
  end

  # Many times test data files have multiple tests. (These are usually files
  # that I've created based on multiple test cases provided by the problem
  # description and/or my needs.) The first line will start with '#' and the
  # data/input for the test is the following lines up to the next '#' or
  # EOF. This method returns a list of two-element lists where the first
  # element is the '#' line, minus the '#' and any leading whitespace, and
  # the second element is the array of strings contains the data lines for
  # that test.
  def test_chunks(part_number = @part_number)
    chunks = []
    chunk_index = -1
    data_lines(part_number).each do |line|
      if line[0] == '#'
        chunk_index += 1
        chunks[chunk_index] = [line[1..-1].strip, []]
      elsif chunk_index >= 0
        chunks[chunk_index][1] << line
      end
    end
    chunks
  end
end
