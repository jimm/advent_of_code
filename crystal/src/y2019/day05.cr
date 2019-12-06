require "../util"

class Day05
  @initial_memory : Array(Int32)

  def initialize(@part_number : Int32, @testing : Bool)
    lines = Util.data_file_lines(
      2019, 5,
      @part_number == 1 ? 1 : (@testing ? 2 : 1),
      @part_number == 1 ? false : @testing
    )
    @initial_memory = lines[0].split(",").map { |s| s.to_i }
    @computer = IntcodeComputer.new
    @computer.load(@initial_memory)
  end

  def run
    if @part_number == 1
      part1()
    else
      part2()
    end
  end

  def part1
    @computer.trace(true) if @testing
    @computer.enqueue_input(1)
    @computer.run
  end

  def part2
    if @testing
      {5 => 999, 8 => 1000, 10 => 1001}.each do |input, expected|
        @computer.load(@initial_memory)
        @computer.enqueue_input(input)
        print("expect to see #{expected}: ")
        @computer.run
      end
    else
      @computer.enqueue_input(5)
      @computer.run
    end
  end
end

AoC.register("2019.5", ->(part_number : Int32, testing : Bool) do
  Day05.new(part_number, testing).run
end)
