# Snailfish

class Day18 < Day
  class SnailfishNumber
    attr_accessor :parent, :left, :right, :is_left

    def self.from_array(arr, parent = nil)
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
      n = SnailfishNumber.new(left, right)
      left.parent = n
      right.parent = n
      n
    end

    def initialize(left, right)
      @left = left
      @left.is_left = true
      @right = right
      @right.is_left = false
      @parent = nil
    end

    def ==(other)
      @left == other.left && @right == other.right
    end

    # Returns a new SnailfishNumber that is the sum of this one and `other`.
    def add(other)
      SnailfishNumber.new(self, other).reduce
    end

    def magnitude
      3 * @left.magnitude + 2 * @right.magnitude
    end

    def integer?
      false
    end

    def number?
      !integer?
    end

    def left?
      @is_left
    end

    def right?
      !left?
    end

    def number_to_left
      if right? && @parent
        left = @parent&.left
        return left if left&.integer?

        return left.last_number
      end

      @parent&.number_to_left
    end

    def number_to_right
      if left? && @parent
        right = @parent.right
        return right if right&.integer?

        return right.first_number
      end

      @parent&.number_to_right
    end

    def first_number
      @left.first_number
    end

    def last_number
      @right.last_number
    end

    def to_a
      [@left.to_a, @right.to_a]
    end

    def to_s
      to_a.to_s
    end

    def inspect
      to_s
    end

    def reduce
      while true
        deep_one = find_first_deep_child(0)
        if deep_one
          deep_one.explode
          next
        end

        deep_ten = find_first_large_integer
        break unless deep_ten

        parent = deep_ten.parent
        if deep_ten == parent.left
          parent.left = deep_ten.split
        else
          parent.right = deep_ten.split
        end
      end
      self
    end

    # Replaces `old_child` with `new_child` and returns the new child.
    def replace(old_child, new_child)
      if old_child == @left
        @left = new_child
        @left.is_left = true
      else
        @right = new_child
        @right.is_left = false
      end
      new_child.parent = self
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

    def split
      self
    end
  end

  class SnailfishInteger < SnailfishNumber
    attr_accessor :value

    def initialize(val)
      @value = val
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

    def explode
      self
    end

    def split
      @parent.replace(self,
                      SnailfishNumber.new(
                        SnailfishInteger.new(@value / 2),
                        SnailfishInteger.new((@value.to_f / 2.0).ceil)
                      ))
    end
  end

  def part1
    lines = data_lines(1)
  end

  def part1_tests
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

    run_chunk_tests(1) do |expected, lines|
      s1, s2 = expected.split(':')
      expected_magnitude = s1.to_i
      expected_sum = parse_sailfish_num(s2)

      sum = lines.map { |line| parse_sailfish_num(line) }
                 .reduce do |sum, sn|
        warn "sum = #{sum}" # DEBUG
        warn "adding sn = #{sn}" # DEBUG
        sum.add(sn)
      end
      magnitude = sum.magnitude

      puts sum
      puts expected_sum
      puts magnitude
      puts expected_magnitude

      [
        sum == expected_sum && magnitude == expected_magnitude,
        [sum, magnitude]
      ]
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
      puts "  error: after explode dest = #{dest}"
      return
    end

    puts '  ok'
  end

  def part2
    lines = data_lines(1)
  end

  def parse_sailfish_num(line)
    SnailfishNumber.from_array(eval(line))
  end
end
