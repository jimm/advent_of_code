require "../util"

class Day
  @day : Int32

  def initialize(@part_number : Int32, @testing : Bool)
    @day = self.class.name[3..].to_i
  end

  def data_lines(part_number = @part_number, testing = @testing)
    Util.data_file_lines(2019, @day, part_number, testing)
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
end
