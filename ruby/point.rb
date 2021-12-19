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

  def to_s
    "(#{x}, #{y}, #{z})"
  end

  def inspect
    "Point#{self}"
  end
end
