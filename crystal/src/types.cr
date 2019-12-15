struct Point2
  getter x, y

  def initialize(@x : Int32, @y : Int32)
  end

  def initialize(loc : {Int32, Int32})
    @x = loc[0]
    @y = loc[1]
  end

  def +(other : Point2)
    Point2.new(@x + other.x, @y + other.y)
  end

  def +(other : {Int32, Int32})
    Point2.new(@x + other[0], @y + other[1])
  end

  def -(other : Point2)
    Point2.new(@x - other.x, @y - other.y)
  end

  def -(other : {Int32, Int32})
    Point2.new(@x - other[0], @y - other[1])
  end

  def to_tuple
    {@x, @y}
  end

  def hash
    to_tuple.hash
  end
end

struct Point3
  getter x, y, z

  def initialize(@x : Int32, @y : Int32, @z : Int32)
  end

  def +(other : Point3)
    Point3.new(@x + other.x, @y + other.y, @z + other.z)
  end

  def +(other : {Int32, Int32, Int32})
    Point3.new(@x + other[0], @y + other[1], @z + other[2])
  end

  def -(other : Point3)
    Point3.new(@x - other.x, @y - other.y, @z - other.z)
  end

  def -(other : {Int32, Int32, Int32})
    Point3.new(@x - other[0], @y - other[1], @z - other[2])
  end

  def to_tuple
    {@x, @y}
  end

  def hash
    to_tuple.hash
  end
end
