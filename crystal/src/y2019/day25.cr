require "../day"

module Year2019
  class Day25 < Day
    def part1
      computer = IntcodeComputer.new("game")
      program = data_lines(testing: false)[0].split(",").map(&.to_i64)
      computer.load(program)

      if @testing
        computer.dump_memory(true)
        return
      end

      computer.direct_output_to('c')
      spawn {
        computer.run
        exit(0)
      }
      inventory = collect_parts(computer)
      pass_pressure_plate(computer, inventory)
    end

    def part2
      no_tests
    end

    def collect_parts(computer)
      instructions = [
        "south",
        "take fixed point",
        "north",
        "north",
        "take spool of cat6",
        "north",
        "take monolith",
        "north",
        "take hypercube",
        "south",
        "west",
        "take planetoid",
        "east",
        "south",
        "east",
        "north",
        "take candy cane",
        "east",
        # "take giant electromagnet",
        "west",
        "south",
        "east",
        "take easter egg",
        "east",
        "south",
        "take ornament", # w, s from here (Engineering), then w to plate
        "west",
        "south",
      ]
      Fiber.yield
      instructions.each do |instruction|
        instruction.each_char { |ch| computer.append_input(ch.ord.to_i64) }
        computer.append_input(10)
        Fiber.yield
      end
      instructions
        .select { |i| i.starts_with?("take") }
        .map { |i| i[5..] }
    end

    def pass_pressure_plate(computer, inventory)
      puts(inventory)
    end
  end
end

AoC.register(Year2019::Day25)
