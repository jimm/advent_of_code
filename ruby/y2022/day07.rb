#!/usr/bin/env ruby
#
# No Space Left On Device

require_relative '../day'

class MyINode
  attr_accessor :name, :size, :parent

  def initialize(name)
    @parent = nil
    @name = name
    @size = 0
  end

  def root?
    @parent.nil?
  end

  def directory?
    false
  end

  def file?
    false
  end
end

class MyDirectory < MyINode
  include Enumerable

  attr_accessor :inodes

  def initialize(name)
    super
    @inodes = []
  end

  def directory?
    true
  end

  # Yields all directories from this one down.
  def each_dir_recursive(&block)
    block.call(self)
    @inodes.each do |inode|
      inode.each_dir_recursive(&block) if inode.directory?
    end
  end

  def add_inode(inode)
    return if inode.file? && @inodes.detect { _1.file? && _1.name == inode.name }
    return if inode.directory? && @inodes.detect { _1.directory? && _1.name == inode.name }

    @inodes << inode
    inode.parent = self
    add_size(inode.size) if inode.instance_of?(MyFile)
  end

  def subdir(name)
    @inodes.detect { |i| i.name == name && i.directory? }
  end

  def add_size(size)
    @size += size
    parent.add_size(size) unless root?
  end

  def print_tree(prefix = '')
    puts "#{prefix}- #{name} (dir, size=#{@size})"
    @inodes.sort_by(&:name).each { _1.print_tree(prefix + '  ') }
  end
end

class MyFile < MyINode
  attr_reader :size

  def initialize(name, size)
    super(name)
    @size = size
  end

  def file?
    true
  end

  def print_tree(prefix = '')
    puts "#{prefix}- #{name} (file, size=#{@size})"
  end
end

class Day07 < Day
  PART1_MAX_SIZE = 100_000
  TOTAL_DISK_SPACE = 70_000_000
  UNUSED_SPACE_NEEDED = 30_000_000

  def part1
    puts do_part1(data_lines(1))
  end

  def part1_tests
    do_tests
  end

  def part2
    puts do_part2(data_lines(1))
  end

  def part2_tests
    do_tests
  end

  private

  def do_part1(lines)
    root = build_file_system(lines)
    sum = 0
    root.each_dir_recursive do |dir|
      sum += dir.size if dir.size <= PART1_MAX_SIZE
    end
    sum
  end

  # ddbcvhqr is wrong
  def do_part2(lines)
    root = build_file_system(lines)
    size_needed = UNUSED_SPACE_NEEDED - (TOTAL_DISK_SPACE - root.size)

    smallest_dir_big_enough = root
    root.each_dir_recursive do |dir|
      smallest_dir_big_enough = dir if dir.size >= size_needed && smallest_dir_big_enough.size > dir.size
    end
    smallest_dir_big_enough.size
  end

  def do_tests
    run_chunk_tests(1) do |expected, lines|
      expected = expected.split(',')[@part_number - 1].to_i
      answer = send("do_part#{@part_number}".to_sym, lines)
      [answer == expected, answer, expected]
    end
  end

  def build_file_system(lines)
    root = MyDirectory.new('/')
    cwd = root
    lines.each do |line|
      case line
      when /^\$ cd (.*)/
        dir = ::Regexp.last_match(1)
        cwd = case dir
              when '/'
                root
              when '..'
                cwd.parent
              else
                cwd.subdir(dir)
              end
      when '$ ls'
        # nop
      when /^dir (.*)/
        cwd.add_inode(MyDirectory.new(::Regexp.last_match(1)))
      when /^(\d+) (.*)/
        cwd.add_inode(MyFile.new(::Regexp.last_match(2), ::Regexp.last_match(1).to_i))
      end
    end
    root
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc
end
