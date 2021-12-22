# Reactor Reboot

require 'set'

class Day22 < Day
  class Solid
    attr_reader :state, :x, :y, :z

    def initialize(state, x_range, y_range, z_range)
      @state = state
      @x = x_range
      @y = y_range
      @z = z_range
      @subtracted = []
    end

    def subtract(solid)
      @subtracted << solid
    end

    def volume
      vol = 0
      @x.each do |x|
        @y.each do |y|
          @z.each do |z|
            vol += 1 if contains?(x, y, z)
          end
        end
      end
      vol
    end

    def contains?(x, y, z)
      @subtracted.each do |s|
        return false if s.x.include?(x) && s.y.include?(y) && s.z.include?(z)
      end
      @x.include?(x) && @y.include?(y) && @z.include?(z)
    end

    def intersects?(other)
      @x.min <= other.x.max && @y.min <= other.y.max && @z.min <= other.z.max &&
        @x.max >= other.x.min && @y.max >= other.y.min && @z.max >= other.z.min
    end

    # Returns a new Solid or nil. The brick's state will be nil.
    def intersection(other)
      return nil unless intersects?(other)

      Solid.new(nil,
                ([@x.min, other.x.min].max..[@x.max, other.x.max].min),
                ([@y.min, other.y.min].max..[@y.max, other.y.max].min),
                ([@z.min, other.z.min].max..[@z.max, other.z.max].min))
    end
  end

  def part1
    init_bricks = parse(data_lines(1), -50, 50)
    on_bricks = run_init(init_bricks)
    puts on_bricks.map(&:volume).sum
  end

  def part1_tests
    tests(1, -50, 50)
  end

  def part2
    init_bricks = parse(data_lines(1), nil, nil)
    on_bricks = run_init(init_bricks)
    puts on_bricks.map(&:volume).sum
  end

  def part2_tests
    tests(2, nil, nil)
  end

  def tests(part, min_val, max_val)
    run_chunk_tests(part) do |expected, lines|
      expected = eval(expected)[:on]
      init_bricks = parse(lines, min_val, max_val)
      on_bricks = run_init(init_bricks)
      num_on_pixels = on_bricks.map(&:volume).sum
      [expected == num_on_pixels, num_on_pixels]
    end
  end

  def run_init(init_bricks)
    prev_bricks = []
    init_bricks.each do |brick|
      prev_bricks.each do |b|
        i_brick = b.intersection(brick)
        b.subtract(i_brick) if i_brick
      end
      prev_bricks << brick if brick.state == :on
    end
    prev_bricks
  end

  def parse(lines, min_val, max_val)
    lines.map do |line|
      line =~ /(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)/
      state = Regexp.last_match(1).to_sym
      xyz = xyz_ranges(min_val, max_val, *(2..7).map { |i| Regexp.last_match(i).to_i })
      xyz ? Solid.new(state, *xyz) : nil
    end
         .compact
  end

  def xyz_ranges(min_val, max_val, xmin, xmax, ymin, ymax, zmin, zmax)
    return [(xmin..xmax), (ymin..ymax), (zmin..zmax)] if min_val.nil? && max_val.nil?

    return nil if xmin > max_val || ymin > max_val || zmin > max_val ||
                  xmax < min_val || ymax < min_val || zmax < min_val

    [
      ([xmin, min_val].max..[xmax, max_val].min),
      ([ymin, min_val].max..[ymax, max_val].min),
      ([zmin, min_val].max..[zmax, max_val].min)
    ]
  end
end
