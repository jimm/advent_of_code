require "../day"

module Year2019
  class Day01 < Day
    def part1
      puts(masses().sum { |mass| fuel_for_mass(mass) })
    end

    def part2
      puts(masses().sum { |mass| recursive_fuel_for_mass(mass) })
    end

    def masses
      data_lines(part_number: 1, testing: false).map(&.to_i)
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
end

AoC.register(Year2019::Day01)
