require "../util"

class Day02
  getter computer = IntcodeComputer.new

  def run(part_number : Int32, testing : Bool)
    lines = Util.data_file_lines(2019, 2, 1, testing)
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
    if !testing
      @computer.set(1, 12)
      @computer.set(2, 2)
    end
    @computer.run
    puts(@computer.get(0))
  end

  def part2(initial_memory, testing)
    (0...99).each do |noun|
      (0..99).each do |verb|
        @computer.load(initial_memory)
        @computer.set(1, noun)
        @computer.set(2, verb)
        @computer.run
        if @computer.get(0) == 19690720
          puts(100 * noun + verb)
          return
        end
      end
    end
    puts("failed: no noun/verb found that produces magic number")
  end
end

AoC.register("2019.2", ->(part_number : Int32, testing : Bool) do
  Day02.new.run(part_number, testing)
end)
