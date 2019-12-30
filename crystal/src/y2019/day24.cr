require "../day"
require "./grid_map"

module Year2019
  class Day24 < Day
    class Biome < GridMap(Char)
      def biodiversity
        bitmask = 0_u32
        5.times do |y|
          5.times do |x|
            if get(x, y) == '#'
              bitmask |= 1 << (y * 5) + x
            end
          end
        end
        bitmask
      end

      def num_adjacent_bugs(x, y)
        count = 0
        [{x, y - 1}, {x, y + 1}, {x - 1, y}, {x + 1, y}].each do |xy|
          count += 1 if get(xy, '.') == '#'
        end
        count
      end

      def bug_count
        self.select { |xy| get(xy, '.') == '#' }.size
      end

      def evolve
        new_biome = Biome.new
        5.times do |y|
          5.times do |x|
            count = num_adjacent_bugs(x, y)
            val = get(x, y, '.')
            if val == '#'
              new_val = (count == 1) ? '#' : '.'
            elsif val == '.' && (count == 1 || count == 2)
              new_val = '#'
            else
              new_val = val
            end
            new_biome.set(x, y, new_val)
          end
        end
        each { |xy| set(xy, new_biome.get(xy, '.')) }
      end

      def to_s
        String.build do |str|
          5.times do |y|
            5.times do |x|
              str << get(x, y)
            end
            str << '\n'
          end
        end
      end
    end

    class RecursiveBiome < Biome
    end

    def part1
      biome = load_biome()
      biodiversity = biome.biodiversity
      progress(biome, biodiversity) if @testing
      history = Set.new([biodiversity])
      while true
        biome.evolve
        biodiversity = biome.biodiversity
        progress(biome, biodiversity) if @testing
        if history.includes?(biodiversity)
          if @testing
            if biodiversity != 2129920
              puts("error: saw #{"%08x" % biodiversity} but expected #{"%08x" % 2129920}")
            else
              puts("ok")
            end
          else
            puts(biodiversity)
          end
          return
        end
        history.add(biodiversity)
      end
    end

    def part2
      biome = load_biome()
      num_minutes = @testing ? 10 : 200
      num_minutes.times { biome.evolve }
      puts(biome.bug_count)
    end

    def load_biome
      Biome.new.tap do |biome|
        data_lines().each_with_index do |line, y|
          line.each_char_with_index do |ch, x|
            biome.set(x, y, ch)
          end
        end
      end
    end

    def progress(biome, biodiversity)
      puts()
      puts(biome.to_s)
      puts("%08x" % biodiversity)
    end
  end
end

AoC.register(Year2019::Day24)
