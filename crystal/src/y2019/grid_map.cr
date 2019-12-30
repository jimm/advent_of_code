class GridMap(T)
  include Enumerable({Int32, Int32})

  property map : Hash({Int32, Int32}, T)
  getter min_x, min_y, max_x, max_y : Int32

  def initialize
    @map = Hash({Int32, Int32}, T).new
    @min_x = @min_y = Int32::MAX
    @max_x = @max_y = Int32::MIN
  end

  def set(xy : {Int32, Int32}, val)
    @map[xy] = val
    x, y = xy
    @min_x = x if x < @min_x
    @min_y = y if y < @min_y
    @max_x = x if x > @max_x
    @max_y = y if y > @max_y
  end

  def set(x : Int32, y : Int32, val)
    set({x, y}, val)
  end

  def get(xy : {Int32, Int32}, default = 0)
    if @map.has_key?(xy)
      @map[xy]
    else
      default
    end
  end

  def get(x : Int32, y : Int32, default = 0)
    get({x, y}, default)
  end

  def width
    @max_x - @min_x + 1
  end

  def height
    @max_y - @min_y + 1
  end

  # Yields to a block with each key ({x, y} coord) in y, x order (row,
  # column).
  def each
    @map.keys
      .sort_by { |xy| xy[1] * @max_y + xy[0] }
      .each { |xy| yield xy }
  end

  # Assumes map is rectangular and that values are character codes.
  def to_s
    String.build do |str|
      (@min_y..@max_y).each do |y|
        (@min_x..@max_x).each do |x|
          str << get(x, y).chr
        end
        str << "\n"
      end
    end
  end
end
