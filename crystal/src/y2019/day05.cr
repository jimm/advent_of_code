require "../util"

class Day05
  getter computer = IntcodeComputer.new

  def run(part_number : Int32, testing : Bool)
    lines = Util.data_file_lines(2019, 5,
      part_number == 2 && testing ? 2 : 1,
      part_number == 1 ? true : testing)
    initial_memory = lines[0].split(",").map { |s| s.to_i }
    proc = if part_number == 1
             ->{ part1(initial_memory, testing) }
           else
             ->{ part2(initial_memory, testing) }
           end
    proc.call
  end

  def part1(initial_memory, testing)
    @computer.load(initial_memory)
    @computer.trace(true) if testing
    @computer.enqueue_input(1)
    @computer.run
  end

  def part2(initial_memory, testing)
    @computer.load(initial_memory)
    if testing
      [5, 8, 10].each do |input|
        @computer.load(initial_memory)
        @computer.enqueue_input(input)
        @computer.run
      end
    else
      @computer.enqueue_input(5)
      @computer.run
    end
  end
end

AoC.register("2019.5", ->(part_number : Int32, testing : Bool) do
  Day05.new.run(part_number, testing)
end)
