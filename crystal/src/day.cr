macro no_tests
  if @testing
    puts("no tests defined for part #{@part_number}")
    return
  end
end

class Day
  @year : Int32
  @day : Int32

  def initialize(@part_number : Int32, @testing : Bool)
    year_day = /Year(\d+)::Day(\d+)/
    year_day.match(self.class.name)
    @year = $1.to_i
    @day = $2.to_i
  end

  def run
    if @part_number == 1
      part1()
    else
      part2()
    end
  end

  def part1
    raise "part1 not implemented"
  end

  def part2
    raise "part2 not implemented"
  end

  def data_lines(part_number = @part_number, testing = @testing)
    day_str = sprintf("%02d", @day)
    testing_str = testing ? "_test" : ""
    path = "../data/y#{@year}/day#{day_str}_#{part_number}#{testing_str}.txt"
    File.read_lines(path).tap(&.delete(""))
  end

  # Many times test data files have multiple tests. The first line will
  # start with '#' and the data/input for the test is the following lines up
  # to the next '#' or EOF. This method returns an Array({String,
  # Array(String)}) where the first string in the tuple is the '#' line,
  # minus the '#' and any leading whitespace and the array of strings
  # contains the data lines for that test.
  def data_chunks(lines)
    lines
      .chunks { |line| line[0] == '#' }
      .in_groups_of(2, {true, [""]})
      .map { |tuple_pair|
        first_line_tuple, remaining_lines_tuple = tuple_pair
        first_line = first_line_tuple[1][0]
        {first_line_tuple[1][0].lchop("# "), remaining_lines_tuple[1]}
          .as({String, Array(String)})
      }
  end
end
