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
  end

  class Day12 < Day
    def part1
      if @testing
        ok = true
        data_lines().in_groups_of(5, "").each do |line_group|
          ok &&= run_test1(line_group[0], line_group[1..])
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
  end
end

AoC.register(Year2019::Day12)
