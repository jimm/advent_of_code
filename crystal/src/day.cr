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
end
