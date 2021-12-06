# Lanternfish

class Day06 < Day
  def part1
    do_part(80)
  end

  def part2
    do_part(256)
  end

  def do_part(num_generations)
    lines = data_lines(1)
    fish = lines.first.split(',').map(&:to_i)
    fish = simulate_generations(fish, num_generations)
    puts fish.length
  end

  def simulate_generations(fish, n)
    n.times do
      fish = simulate_generation(fish)
    end
    fish
  end

  def simulate_generation(fish)
    new_fish = []
    num_births = 0
    fish.each do |f|
      case f
      when 0
        new_fish << 6
        num_births += 1
      else
        new_fish << f - 1
      end
    end
    new_fish += [8] * num_births
    new_fish
  end
end
