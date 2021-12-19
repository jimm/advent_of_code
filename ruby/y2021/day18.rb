# Snailfish

class Day18 < Day
  class SnailfishNumber
    attr_accessor :parent, :is_left

    def self.from_array(arr)
      left = if arr[0].instance_of?(Integer)
               SnailfishInteger.new(arr[0])
             else
               from_array(arr[0])
             end
      right = if arr[1].instance_of?(Integer)
                SnailfishInteger.new(arr[1])
              else
                from_array(arr[1])
              end
      SnailfishPair.new(left, right)
    end

    def left?
      @is_left
    end

    def right?
      !@is_left
    end

    def to_s
      self.class.name
    end

    def inspect
      to_s
    end
  end


  class SnailfishPair < SnailfishNumber
    attr_accessor :left, :right

    def initialize(left, right)
      @left = left
      @left.is_left = true
      @left.parent = self

      @right = right
      @right.is_left = false
      @right.parent = self
    end

    def ==(other)
      return false unless self.class == other.class
      (@left.object_id == other.left.object_id || @left == other.left) &&
        (@right.object_id == other.right.object_id || @right == other.right)
    end

    # Returns a new SnailfishPair that is the sum of this one and `other`.
    def add(other)
      SnailfishPair.new(self, other).reduce
    end

    def magnitude
      3 * @left.magnitude + 2 * @right.magnitude
    end

    def integer?
      false
    end

    def number_to_left
      raise "parent of self" if @parent&.object_id == object_id # check
      return nil unless @parent
      return @parent.left.last_number if right?
      @parent.number_to_left
    end

    def number_to_right
      raise "parent of self" if @parent&.object_id == object_id # check
      return nil unless @parent
      return @parent.right.first_number if left?
      @parent.number_to_right
    end

    def first_number
      @left.first_number
    end

    def last_number
      @right.last_number
    end

    def reduce
      while true
        deep_one = find_first_deep_child(0)
        if deep_one
          deep_one.explode
          next
        end

        deep_ten = find_first_large_integer
        if deep_ten
          deep_ten.split
        else
          break
        end
      end
      self
    end

    # Replaces `old_child` with `new_child` and returns the new child.
    def replace(old_child, new_child)
      if @left.object_id == old_child.object_id
        @left = new_child
        @left.is_left = true
      elsif @right.object_id == old_child.object_id
        @right = new_child
        @right.is_left = false
      else
        raise "old child is not existing child"
      end
      new_child.parent = self
      # just in case
      old_child.parent = nil
    end

    def find_first_deep_child(level)
      return self if level == 4

      @left.find_first_deep_child(level + 1) ||
        @right.find_first_deep_child(level + 1)
    end

    def find_first_large_integer
      @left.find_first_large_integer || @right.find_first_large_integer
    end

    def explode
      l = number_to_left
      r = number_to_right
      l.value += @left.value if l
      r.value += @right.value if r
      @parent.replace(self, SnailfishInteger.new(0))
    end
  end


  class SnailfishInteger < SnailfishNumber
    attr_accessor :value

    def initialize(val)
      @value = val
      @parent = nil
    end

    def ==(other)
      other&.integer? && other.value == value
    end

    def magnitude
      @value
    end

    def integer?
      true
    end

    def number_to_left
      self
    end

    def number_to_right
      self
    end

    def find_first_deep_child(_)
      nil
    end

    def find_first_large_integer
      @value >= 10 ? self : nil
    end

    def first_number
      self
    end

    def last_number
      self
    end

    def to_a
      @value
    end

    def to_s
      @value.to_s
    end

    def split
      left = SnailfishInteger.new(@value / 2)
      right = SnailfishInteger.new((@value.to_f / 2.0).ceil)
      sn = SnailfishPair.new(left, right)
      left.parent = sn
      right.parent = sn
      @parent.replace(self, sn)
    end
  end

  # ================================================================

  def part1
    sum = data_lines(1)
            .map { |line| parse_snailfish_num(line) }
            .reduce { |sum, sn| sum.add(sn) }
    magnitude = sum.magnitude
    puts magnitude
  end

  def part1_tests
    test_find_and_split([[[[0,7],4],[15,[0,13]]],[1,1]],
                        [[[[0,7],4],[[7,8],[0,13]]],[1,1]],
                        15)
    test_find_and_split([[[[0,7],4],[[7,8],[0,13]]],[1,1]],
                        [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]],
                        13)

    test_explode([[[[[9, 8], 1], 2], 3], 4],
                 [[[[0, 9], 2], 3], 4],
                 [9, 8], nil, 1)
    test_explode([7, [6, [5, [4, [3, 2]]]]],
                 [7, [6, [5, [7, 0]]]],
                 [3, 2], 4, nil)
    test_explode([[6, [5, [4, [3, 2]]]], 1],
                 [[6, [5, [7, 0]]], 3],
                 [3, 2], 4, 1)
    test_explode([[3, [2, [1, [7, 3]]]], [6, [5, [4, [3, 2]]]]],
                 [[3, [2, [8, 0]]], [9, [5, [4, [3, 2]]]]],
                 [7, 3], 1, 6)
    test_explode([[3, [2, [8, 0]]], [9, [5, [4, [3, 2]]]]],
                 [[3, [2, [8, 0]]], [9, [5, [7, 0]]]],
                 [3, 2], 4, nil)

    # magnitude
    puts
    sn = SnailfishNumber.from_array([[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]])
    magnitude = sn.magnitude
    if magnitude == 3488
      puts 'magnitude check ok'
    else
      puts "magnitude check error: expected 3488, got #{magnitude}"
    end

    test_reduce([[[[[4,3],4],4],[7,[[8,4],9]]], [1,1]],
                [[[[0,7],4],[[7,8],[6,0]]],[8,1]])

    run_chunk_tests(1) do |expected, lines|
      expected = eval(expected)
      expected[:sum] = SnailfishNumber.from_array(expected[:sum])

      sum = lines.map { |line| parse_snailfish_num(line) }
              .reduce { |sum, sn| sum.add(sn) }
      magnitude = sum.magnitude
      answer = {sum: sum, magnitude: expected[:magnitude] ? magnitude : nil}
      [answer == expected, answer]
    end
  end

  def test_explode(src, dest, pair, left_num, right_num)
    puts
    puts
    puts "test_explode #{src}"
    start = SnailfishNumber.from_array(src)
    expected = SnailfishNumber.from_array(dest)

    deep_one = start.find_first_deep_child(0)
    if deep_one.nil? || deep_one.left.value != pair[0] || deep_one.right.value != pair[1]
      puts "  error: expected pair #{pair} but found #{deep_one}"
      return
    end

    l = deep_one.number_to_left&.value
    r = deep_one.number_to_right&.value
    if l != left_num || r != right_num
      puts "  error: expected left #{left_num}, right #{right_num} but got left #{l}, right #{r}"
      return
    end

    deep_one.explode
    if start != expected
      puts "  error: after explode start = #{start}, expected #{expected}"
      return
    end

    puts '  ok'
  end

  def test_find_and_split(src, dest, first_large)
    puts
    puts "test_find_and_split #{src}"
    start = SnailfishNumber.from_array(src)
    expected = SnailfishNumber.from_array(dest)

    deep_ten = start.find_first_large_integer
    if deep_ten.value != first_large
      puts "  error: expected first large #{first_large}, found #{deep_ten}"
      return
    end

    deep_ten.split
    if start != expected
      puts "  error: after explode start = #{start}, expected #{expected}"
    end

    puts '  ok'
  end

  def test_reduce(src, dest)
    puts
    puts "test_reduce #{src}"
    start = SnailfishNumber.from_array(src)
    expected = SnailfishNumber.from_array(dest)

    if start.reduce != expected
      puts "  error: after reduce start = #{start}, expected #{expected}"
    end

    puts '  ok'
  end

  # 4490 too high
  def part2
    numbers = data_lines(1).map { |line| parse_snailfish_num(line) }
    max_magnitude = 0
    numbers.combination(2) do |combination|
      a, b = *combination
      mag = a.add(b).magnitude
      max_magnitude = mag if mag > max_magnitude
      mag = b.add(a).magnitude
      max_magnitude = mag if mag > max_magnitude
    end
    puts max_magnitude
  end

  def part2_tests
    run_chunk_tests(2) do |expected, lines|
      numbers = lines.map { |line| parse_snailfish_num(line) }
      max_magnitude = 0
      numbers.combination(2) do |combination|
        a, b = *combination

        mag = a.add(b).magnitude
        max_magnitude = mag if mag > max_magnitude

        mag = b.add(a).magnitude
        max_magnitude = mag if mag > max_magnitude
      end
      [max_magnitude == expected.to_i, max_magnitude]
    end
  end

  def parse_snailfish_num(line)
    SnailfishNumber.from_array(eval(line))
  end
end
