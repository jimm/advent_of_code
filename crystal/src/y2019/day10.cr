require "../day"

module Year2019
  alias Asteroid = Tuple(Int32, Int32)
  alias Angle = Float64

  class Day10 < Day
    class AsteroidMap
      getter rows : Int32
      getter cols : Int32
      getter asteroids : Array(Asteroid)
      getter map_strings : Array(String)

      def initialize(@map_strings)
        @rows = map_strings.size
        @cols = map_strings[0].size
        @asteroids = [] of Asteroid
        @map_strings.each_with_index do |row_str, y|
          row_str.each_char_with_index do |char, x|
            if char != '.'
              @asteroids << {x, y}
            end
          end
        end
      end

      def asteroid_char_at(a : Asteroid)
        @map_strings[a[1]].char_at(a[0])
      end
    end

    class Test
      getter best : Asteroid
      getter other_count : Int32
      getter map : AsteroidMap

      def initialize(@best, @other_count, @map)
      end
    end

    def part1
      if @testing
        ok = true
        tests = parse_test1_input()
        tests.each do |tg|
          result = run_test1(tg)
          ok &&= result
        end
        puts("ok") if ok # errors already printed
      else
        map = AsteroidMap.new(data_lines())
        winner = part1_find_max_seen(map)
        puts(winner[1])
      end
    end

    def part2
      if @testing
        expected, map = parse_test2_input()
        max_index = expected.map { |e| e[0] }.max
        vaporized = vaporize_sweep(map, max_index + 1)

        ok = true
        expected.each do |index, asteroid|
          found = vaporized[index]
          if found != asteroid
            puts("error: asteroid vaporized at index #{index} expected #{asteroid} saw #{found}")
            ok = false
          end
        end
        puts("ok") if ok
      else
        map = AsteroidMap.new(data_lines(part_number: 1))
        vaporized = vaporize_sweep(map, 200)
        asteroid_bet = vaporized[199]
        puts(asteroid_bet[0] * 100 + asteroid_bet[1])
      end
    end

    # Vaporizes up to *max_vaporization_count* asteroids in *map* and
    # returns them in the order vaporized.
    def vaporize_sweep(map, max_vaporization_count)
      laser_at = part1_find_max_seen(map)[0]
      all_angles, distances = calc_angles_and_distances(map.asteroids)
      angles_to_others = all_angles[laser_at] # Hash(Float64, Asteroid)
      sorted_angles = angles_to_others.keys.sort

      index = 0
      vaporized = [] of Asteroid
      first_angle_found = false
      sorted_angles.cycle do |angle|
        if !first_angle_found
          next if angle != 270.0 # we happen to know that there are ones here
          first_angle_found = true
        end

        next if angles_to_others[angle].empty?

        # find nearest and, if thre is one, vaporize it
        a = angles_to_others[angle].min_by { |a| distances[{laser_at, a}] }
        unless a.nil?
          vaporized << a
          if vaporized.size == max_vaporization_count
            return vaporized
          end
          angles_to_others[angle].delete(a)
        end
        index += 1
      end
    end

    def angle_between(p1, p2)
      x_len = (p2[0] - p1[0]).to_f
      y_len = (p2[1] - p1[1]).to_f
      arctan_to_degrees(Math.atan2(y_len, x_len))
    end

    def arctan_to_degrees(x)
      (x > 0.0 ? x : (2.0*Math::PI + x)) * 360.0 / (2.0*Math::PI)
    end

    def square_of_distance_between(p1, p2)
      x_len = (p2[0] - p1[0]).abs.to_f
      y_len = (p2[1] - p1[1]).abs.to_f
      x_len * x_len + y_len * y_len
    end

    def calc_angles_and_distances(asteroids) : Tuple(Hash(Asteroid, Hash(Angle, Array(Asteroid))), Hash(Tuple(Asteroid, Asteroid), Float64))
      inner_proc = ->(h : Hash(Angle, Array(Asteroid)), k : Angle) do
        h[k] = [] of Asteroid
      end
      outer_proc = ->(h : Hash(Asteroid, Hash(Angle, Array(Asteroid))), k : Asteroid) do
        h[k] = Hash(Angle, Array(Asteroid)).new(inner_proc)
      end
      angles_between = Hash(Asteroid, Hash(Angle, Array(Asteroid))).new(outer_proc)
      distances_between = {} of Tuple(Asteroid, Asteroid) => Float64

      asteroids.combinations(2).each do |cons|
        a1, a2 = cons

        angles_between[a1][angle_between(a1, a2)] << a2
        angles_between[a2][angle_between(a2, a1)] << a1

        dist = square_of_distance_between(a1, a2)
        distances_between[{a1, a2}] = dist
        distances_between[{a2, a1}] = dist
      end
      {angles_between, distances_between}
    end

    # Returns the `{Asteroid, others_seen}` tuple of the asteroid in *map*
    # that has max `others_seen`.
    def part1_find_max_seen(map)
      asteroids = map.asteroids

      # Calculate angles and distances between asteroids
      angles_between, distances_between = calc_angles_and_distances(asteroids)

      winner = asteroids.map do |a|
        # The simple fact that there is a angle between this asteroid and
        # one or more others means that it can see one of them (the
        # closest). We don't care which is closest right now.
        count_seen = angles_between[a].size
        {a, count_seen}
      end
        .max_by { |a_and_count| a_and_count[1] }
    end

    def run_test1(test)
      winner = part1_find_max_seen(test.map)

      expected = {test.best, test.other_count}
      if winner != expected
        puts("error: expected #{expected} but saw #{winner}")
        return false
      end
      true
    end

    # Reads the test1 input file and returns an array of `Test`s.
    def parse_test1_input
      best_regex = /Best is ([\d,]+) with (\d+) /
      data_lines()
        .chunks { |line| line.starts_with?("Best") }
        .map(&.last)
        .in_groups_of(2)
        .map do |meta_and_map|
          best_regex.match(meta_and_map[0].as(Array(String))[0])
          best = $1.split(",").map(&.to_i)
          other_count = $2.to_i
          Test.new(
            {best[0], best[1]},
            other_count,
            AsteroidMap.new(meta_and_map[1].as(Array(String)))
          )
        end
    end

    # Reads the test2 input file and returns a `Hash(Int32, Asteroid)`
    # and an `AsteroidMap`, together as a `Tuple`.
    def parse_test2_input
      expected_regex = /The (\d+).*is at (\d+),(\d+)/
      expected_chunk, map_chunk = data_lines().chunks { |line| line.starts_with?(";") }
      expected = expected_chunk[1].each_with_object(Hash(Int32, Asteroid).new) do |line, h|
        expected_regex.match(line)
        h[$1.to_i - 1] = {$2.to_i, $3.to_i} # "1" => index 0
      end
      map = AsteroidMap.new(map_chunk[1])
      {expected, map}
    end
  end
end

AoC.register(Year2019::Day10)
