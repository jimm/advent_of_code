require "../day"

module Year2019
  alias Loc3 = {Int32, Int32, Int32}

  class Moon
    property others : Array(Moon)
    getter loc : Loc3
    @velocity : Loc3

    def initialize(@loc)
      @velocity = {0, 0, 0}
      @others = [] of Moon
    end

    def apply_gravity
      dx = 0
      dy = 0
      dz = 0
      @others.each do |other|
        dx += gravity_update(0, other)
        dy += gravity_update(1, other)
        dz += gravity_update(2, other)
      end
      @velocity = {@velocity[0] + dx, @velocity[1] + dy, @velocity[2] + dz}
    end

    def gravity_update(i, other)
      my_val = @loc[i]
      other_val = other.loc[i]
      case
      when other_val > my_val
        1
      when other_val < my_val
        -1
      else
        0
      end
    end

    def apply_velocity
      @loc = {
        @loc[0] + @velocity[0],
        @loc[1] + @velocity[1],
        @loc[2] + @velocity[2],
      }
    end

    def energy
      potential_energy = @loc.map(&.abs).sum
      kinetic_energy = @velocity.map(&.abs).sum
      potential_energy * kinetic_energy
    end

    def to_s
      "pos=<x=#{@loc[0]}, y=#{@loc[1]}, z=#{@loc[2]}>, vel=<x=#{@velocity[0]}, y=#{@velocity[1]}, z=#{@velocity[2]}>"
    end

    def hash
      @loc[0].to_i64 +
        (@loc[1].to_i64 << 10) +
        (@loc[2].to_i64 << 20) +
        (@velocity[0].to_i64 << 30) +
        (@velocity[1].to_i64 << 40) +
        (@velocity[2].to_i64 << 50)
    end
  end

  class Day12 < Day
    def part1
      if @testing
        ok = true
        data_lines().in_groups_of(5, "").each do |line_group|
          result = run_test1(line_group[0], line_group[1..])
          ok &&= result
        end
        puts("ok") if ok # errors already printed if not ok
      else
        moons = make_moons(data_lines())
        1000.times do |_step|
          moons.each(&.apply_gravity)
          moons.each(&.apply_velocity)
        end
        puts(moons.map(&.energy).sum)
      end
    end

    def part2
      if @testing
        lines = data_lines()
        if run_test2(lines[0], lines[1..])
          puts("ok")
        end
      else
        moons = make_moons(data_lines())
        1000.times do |_step|
          moons.each(&.apply_gravity)
          moons.each(&.apply_velocity)
        end
        puts(moons.map(&.energy).sum)
      end
    end

    def make_moons(position_lines)
      loc_regex = /x=(-?\d+), y=(-?\d+), z=(-?\d+)/
      locs = position_lines.map do |line|
        loc_regex.match(line)
        {$1.to_i, $2.to_i, $3.to_i}
      end
      moons = locs.map { |loc| Moon.new(loc) }
      moons.each { |moon| moon.others = moons - [moon] }
      moons
    end

    def universe_state(moons)
      moons.map(&.hash).hash
    end

    def run_test1(expected_line, positions)
      num_steps, expected_energy = expected_line[2..].split(",").map(&.to_i)
      moons = make_moons(positions)

      num_steps.times do |_step|
        moons.each(&.apply_gravity)
        moons.each(&.apply_velocity)
      end

      e = moons.map(&.energy).sum
      if e == expected_energy
        true
      else
        puts("error: energy #{e} != expected energy #{expected_energy}")
        false
      end
    end

    def run_test2(expected_line, positions)
      expected_steps = expected_line[2..].to_u64
      moons = make_moons(positions)
      step = 0_u64
      initial_state = universe_state(moons)
      initial_x = moons[0].loc[0]

      while true
        step += 1
        if step % 1_000_000 == 0
          puts(step)
        end
        if step > expected_steps + 2
          puts("error: expected #{expected_steps} but we've gone beyond that")
          return false
        end
        moons.each(&.apply_gravity)
        moons.each(&.apply_velocity)
        state_x = moons[0].loc[0]
        if state_x == initial_x # optimization: check less frequently
          state = universe_state(moons)
          if state == initial_state
            if step == expected_steps
              return true
            end
            puts("error: found repeat at step #{step} but expected #{expected_steps}")
            return false
          end
        end
      end
    end
  end
end

AoC.register(Year2019::Day12)
