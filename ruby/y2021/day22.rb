# Reactor Reboot

require 'set'

class Day22 < Day
  class Solid
    attr_reader :state, :x, :y, :z, :subtracted

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

    def size
      @x.size * @y.size * @z.size
    end

    def volume
      vol = size
      return vol if @subtracted.empty?
      return vol - @subtracted[0].size if @subtracted.size == 1

      warn
      warn
      warn "calcuating volume of #{@x}, #{@y}, #{@z}" # DEBUG
      independents = @subtracted.select do |sub|
        (@subtracted - [sub]).all? { |osub| !osub.intersects?(sub) }
      end
      warn "  #{@subtracted.size} subs, #{independents.size} independents" # DEBUG

      sub_bounds = Solid.new(:off,
                             (@subtracted.map { |s| s.x.min }.min..@subtracted.map { |s| s.x.max }.max),
                             (@subtracted.map { |s| s.y.min }.min..@subtracted.map { |s| s.y.max }.max),
                             (@subtracted.map { |s| s.z.min }.min..@subtracted.map { |s| s.z.max }.max))
      warn "my size #{vol}, sub_bounds size #{sub_bounds.size}"
      return vol

      sub_bounds.x.each do |x|
        sub_bounds.y.each do |y|
          sub_bounds.z.each do |z|
            vol -= 1 unless contains?(x, y, z)
          end
        end
      end
      vol
    end

    def subtracted?(x, y, z)
      @subtracted.each do |s|
        return true if s.x.include?(x) && s.y.include?(y) && s.z.include?(z)
      end
      false
    end

    def contains?(x, y, z)
      !subtracted?(x, y, z) &&
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
      warn 'on_bricks info' # DEBUG
      on_bricks.each do |ob| # DEBUG
        warn "  ob.size = #{ob.size}, ob.subtracted.size = #{ob.subtracted.size}" # DEBUG
      end
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
