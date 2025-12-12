class Point
  attr_accessor :x, :y, :z

  def initialize(x = 0, y = 0, z = 0)
    @x = x
    @y = y
    @z = z
  end

  ORIGIN = Point.new(0, 0, 0).freeze

  def ==(other)
    @x == other.x && @y == other.y && @z == other.z
  end

  # Compare first x, then y, then z.
  def <=>(other)
    comp = @x <=> other.x
    return comp unless comp == 0

    comp = @y <=> other.y
    return comp unless comp == 0

    @z <=> other.z
  end

  def eql?(other) = self == other

  # Returns a new point whose coordinates are the sum of ours and `other`'s.
  def +(other) = Point.new(@x + other.x, @y + other.y, @z + other.z)

  # When other == nil, this returns the distance squared from the origin.
  def distance_squared(other = nil)
    other ||= ORIGIN
    dx = (other.x - @x).abs
    dy = (other.y - @y).abs
    dz = (other.z - @z).abs
    (dx * dx) + (dy * dy) + (dz * dz)
  end

  # When other == nil, this returns the distance from the origin.
  def distance(other = nil)
    other ||= ORIGIN
    Math.sqrt(distance_squared(other))
  end

  # When other == nil, this returns the manhattan distance from the origin.
  def manhattan_distance(other = nil)
    other ||= ORIGIN
    (other.x - @x).abs + (other.y - @y).abs + (other.z - @z).abs
  end

  def to_a
    [@x, @y, @z]
  end

  def hash
    to_a.hash
  end

  def to_s
    "(#{@x}, #{@y}, #{@z})"
  end

  def inspect
    "Point#{self}"
  end
end

if __FILE__ == $PROGRAM_NAME
  p0 = Point.new(3, 4)
  p1 = Point.new(3, 4, 0)
  unless p0 == p1
    raise "== is not working"
  end

  unless [p0].include?(p1)
    raise "include? is not working"
  end

  unless (p1 <=> p0) == 0
    raise "comparison is not working"
  end

  p0.z = 3
  unless (p1 <=> p0) == -1
    raise "comparison is not working"
  end
end
