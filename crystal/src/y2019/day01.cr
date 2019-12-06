require "../util"

class Day01
  def initialize(@part_number : Int32, @testing : Bool)
  end

  def run
    lines = Util.data_file_lines(2019, 1, 1, false)
    proc = if @part_number == 1
             ->(i : Int32) { fuel_for_mass(i) }
           else
             ->(i : Int32) { recursive_fuel_for_mass(i) }
           end
    puts(lines.reduce(0) { |acc, i| acc + proc.call(i.to_i) })
  end

  def fuel_for_mass(mass)
    fuel = (mass / 3).to_i - 2
    fuel.clamp(0, fuel)
  end

  def recursive_fuel_for_mass(mass)
    sum = 0
    ffm = fuel_for_mass(mass)
    while ffm > 0
      sum += ffm
      ffm = fuel_for_mass(ffm)
    end
    sum
  end
end

AoC.register("2019.1", ->(part_number : Int32, testing : Bool) do
  Day01.new(part_number, testing).run
end)
