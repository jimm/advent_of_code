class Point
  attr_accessor :x, :y, :z

  def initialize(x = 0, y = 0, z = 0)
    @x = x
    @y = y
    @z = z
  end

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

  def distance_squared(other)
    dx = (other.x - @x)
    dy = (other.y - @y)
    dz = (other.z - @z)
    (dx * dx) + (dy * dy) + (dz * dz)
  end

  def distance(other)
    Math.sqrt(distance_squared(other))
  end

  def manhattan_distance(other)
    (other.x - @x).abs + (other.y - @y).abs + (other.z - @z).abs
  end

  def to_a
    [@x, @y, @z]
  end

  def hash
    to_a.hash
  end

  def eql?(other)
    self == other
  end

  def to_s
    "(#{@x}, #{@y}, #{@z})"
  end

  def inspect
    "Point#{self}"
  end
end
