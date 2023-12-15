# A two-dimensional map where each cell is a single value. There is also a
# single convenience "cursor" consisting of writable row and column indexes.
#
# [0, 0] is at the top left of the map. @row increases down, @col increases
# to the right.
class Map
  attr_reader :rows, :height, :width, :wrap_type
  attr_accessor :row, :col

  # Given `lines` of text, initializes a two-dimensional map of characters.
  # Assumes all lines are the same length.
  #
  # `wrap_type` may be nil, :both, :row, or :col.
  def initialize(lines, wrap_type = nil)
    @rows = lines.map { |line| line.split('') }
    @wrap_type = wrap_type
    @height = @rows.length
    @width = @rows[0].length
    @row = @col = 0
  end

  # Converts each cell's character, presumably a single digit char, to an
  # integer.
  def cells_to_ints!
    @height.times do |row|
      @rows[row].map!(&:to_i)
    end
  end

  # Returns the value at [row][col].
  def at(row, col)
    row, col = wrap(row, col)
    return nil unless in_bounds?(row, col)

    @rows[row][col]
  end

  def row(row)
    @rows[row]
  end

  def columns
    (0...@width).map { column(_1) }
  end

  def column(col)
    @rows.map { |row| row[col] }
  end

  # Returns the first occurence of `val` (min row, then min col) as a
  # two-element array containing [row, col], or nil if not found.
  def find(val)
    @rows.each_with_index do |row, row_idx|
      col_idx = row.index(val)
      return [row_idx, col_idx] if col_idx
    end
    nil
  end

  # For each row and column, yields the row number, column number, and
  # value.
  def each
    @rows.each_with_index do |row, ir|
      row.each_with_index do |val, ic|
        yield ir, ic, val
      end
    end
  end

  # Sets value at `row`, `col` to `val`.
  def set(row = @row, col = @col, val)
    row, col = wrap(row, col)
    @rows[row][col] = val
  end

  def delete_row(row = @row)
    return if row < 0 || row >= @height || @height == 0

    @rows.delete_at(row)
    @height -= 1
  end

  def insert_row(index, row)
    @rows.insert(index, row.dup)
    @height += 1
  end

  def delete_col(col = @col)
    return if col < 0 || col >= @width || @width == 0

    @rows.each do |row|
      row.delete_at(col)
    end
    @width -= 1
  end

  def insert_col(index, col)
    @rows.each_with_index do |row, i|
      row.insert(index, col[i].dup)
    end
    @width += 1
  end

  # Calls update! on each cell
  def update_all!
    each do |ir, ic, val|
      @rows[ir][ic] = yield val
    end
  end

  # Updates value at `row`, `col` by yielding its value and storing the
  # result.
  def update!(row = @row, col = @col)
    @rows[row][col] = yield at(row, col)
  end

  # Move to row and col.
  def move_to(row, col)
    @row, @col = wrap(row, col)
  end

  # Move by deltas. wrap_type may be nil, :both, :row, or :col.
  def move_by(row_delta, col_delta)
    @row, @col = wrap(@row + row_delta, @col + col_delta)
  end

  def in_bounds?(row = @row, col = @col)
    row >= 0 && row < @height && col >= 0 && col < @width
  end

  # wrap_type may be nil, :both, :row, or :col.
  def wrap(row, col)
    case wrap_type
    when :both
      row = row % @height
      col = col % @width
    when :row
      row = row % @height
    when :col
      col = col % @width
    end
    [row, col]
  end

  def to_s
    @rows.map(&:join).join("\n")
  end

  # Stretches out each row so that a square map prints closer to a square.
  def to_s_corrected
    @rows.map { |row| row.join(' ') }.join("\n")
  end
end
