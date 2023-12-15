#!/usr/bin/env ruby
#
# Lens Library

require_relative '../day'

class Day15 < Day
  def do_part1(lines)
    box_num_cache = {}
    lines[0].split(',').map { hash_decode(_1, {}) }.sum
  end

  def do_part2(lines)
    boxes = Array.new(256) { Array.new }
    box_num_cache = {}

    # follow commands to set up contents of boxes
    lines[0].split(',').each do |command|
      if command[-1] == '-'
        lens_label = command[..-2]
        box_num = hash_decode(lens_label, box_num_cache)
        delete_lens(boxes[box_num], lens_label)
      else
        lens_label = command[..-3]
        box_num = hash_decode(lens_label, box_num_cache)
        focal_length = command[-1].to_i
        add_or_replace_lens(boxes[box_num], lens_label, focal_length)
      end
    end

    # calculate focusing power
    boxes.map.with_index do |box, i|
      box.map.with_index do |lens_and_focal_length, j|
        (i + 1) * (j + 1) * lens_and_focal_length[1]
      end.sum
    end.sum
  end

  private

  def delete_lens(box, lens_label)
    i = lens_index(box, lens_label)
    box.delete_at(i) if i
  end

  def add_or_replace_lens(box, lens_label, focal_length)
    i = lens_index(box, lens_label)
    if i
      box[i][1] = focal_length
    else
      box << [lens_label, focal_length]
    end
  end

  def lens_index(box, lens_label)
    (0...box.length).each do |i|
      return i if box[i][0] == lens_label
    end
    nil
  end

  def hash_decode(str, cache)
    return cache[str] if cache[str]
    cache[str] = str.chars.inject(0) do |curr, ch|
      x = curr + ch.ord
      ((x << 4) + x) & 0xff
      # ((curr + ch.ord) * 17) & 0xff
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
