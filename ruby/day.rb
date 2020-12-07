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
    fname += "_tests" if @testing
    send(fname.to_sym)
  end

  def part1
    raise "subclasses must implement"
  end

  def part1_tests
    part1
  end

  def part2
    raise "subclasses must implement"
  end

  def part2_tests
    part2
  end

  def no_tests
    if @testing
      puts("no tests available")
      exit(0)
    end
  end

  # ================ reading data files ================

  # Returns the contents of a data file as an array of lines with line
  # endings stripped. File is found using year, day, part number and
  # the testing flag.
  def read_data_file(part_number=@part_number)
    fname = "day#{'%02d' % @day}"
    fname += "_#{part_number}_test" if @testing
    path = File.join(__dir__, "../data/y#{@year}", "#{fname}.txt")
    IO.readlines(path).map(&:chomp!)
  end

  # Returns non-empty lines from the data file for @year, @day, and
  # part_number (default @part_number).
  #
  # Normally, empty lines are skipped but if `skip_empty_lines` is false
  # then they're returned as well.
  def data_lines(part_number=@part_number, skip_empty_lines=true)
    lines = read_data_file(part_number)
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
  def test_chunks(part_number=@part_number)
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
