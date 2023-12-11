#!/usr/bin/env ruby
#
# Pipe Maze

require 'set'
require_relative '../day'
require_relative '../map'

class Day10 < Day
  CORNER_DELTAS = {
    'F' => %i[down right],
    'L' => %i[up right],
    'J' => %i[left up],
    '7' => %i[left down]
  }

  def do_part1(lines)
    map = Map.new(lines)
    start_loc = map.find('S')
    start_char = start_char_at(map, start_loc)
    loop0 = loop_locs(start_loc, start_char, map, 0)
    loop1 = loop_locs(start_loc, start_char, map, 1)
    farthest_distance(loop0, loop1)
  end

  def do_part2(lines)
    map = Map.new(lines)
    start_loc = map.find('S')
    start_char = start_char_at(map, start_loc)
    loop = loop_locs(start_loc, start_char, map, 0)
    count_locs_inside_loop(map, loop, start_loc, start_char)
  end

  private

  def start_char_at(map, start_loc)
    dirs = []
    start_row, start_col = start_loc
    u = map.at(start_row - 1, start_col)
    d = map.at(start_row + 1, start_col)
    r = map.at(start_row, start_col + 1)
    l = map.at(start_row, start_col - 1)
    dirs << :up if ['|', 'F', '7'].include?(u)
    dirs << :down if ['|', 'L', 'J'].include?(d)
    dirs << :left if ['-', 'F', 'L'].include?(l)
    dirs << :right if ['-', 'J', '7'].include?(r)

    case dirs.map(&:to_s).sort
    when %w[down right]
      'F'
    when %w[right up]
      'L'
    when %w[left up]
      'J'
    when %w[down left]
      '7'
    else
      raise 'start char not surrounded by proper chars???'
    end
  end

  def loop_locs(start_loc, start_char, map, start_dir_idx)
    loop_locs = []
    loc = start_loc.dup
    dir = CORNER_DELTAS[start_char][start_dir_idx]
    while loop_locs.empty? || loc != start_loc
      loop_locs << loc.dup
      case dir
      when :up
        loc[0] -= 1
      when :down
        loc[0] += 1
      when :left
        loc[1] -= 1
      when :right
        loc[1] += 1
      end
      ch = map.at(*loc)
      case ch
      when '|'
        dir = dir == :up ? :up : :down
      when '-'
        dir = dir == :left ? :left : :right
      when 'F'
        dir = dir == :up ? :right : :down
      when 'L'
        dir = dir == :down ? :right : :up
      when 'J'
        dir = dir == :down ? :left : :up
      when '7'
        dir = dir == :up ? :left : :down
      end
    end
    loop_locs
  end

  def farthest_distance(loop0, loop1)
    idx = 0
    idx += 1 while idx == 0 || loop0[idx] != loop1[idx]
    idx
  end

  def init_non_loop_chars(map, loop)
    map.each do |ri, ci, ch|
      map.set(ri, ci, '.') if ch != '.' && !loop.include?([ri, ci])
    end
  end

  # We assume that all non-loop chars are '.'.
  def count_locs_inside_loop(map, loop, start_loc, start_char)
    init_non_loop_chars(map, loop)
    id = 'a'
    loc = map.find('.')
    while loc
      flood_fill(map, loop, loc[0], loc[1], id)
      id = id.succ
      loc = map.find('.')
    end
    ids = ('a'...id).to_a

    r, c = start_loc
    inside_ids = Set.new

    # set initial direction and in_dir as if we were coming in to the starting loc
    ch = start_char_at(map, [r, c])
    case ch
    when 'F'
      dir = :left
      in_dir = :down
    when '7'
      dir = :right
      in_dir = :down
    when 'J'
      dir = :right
      in_dir = :up
    when 'L'
      dir = :left
      in_dir = :up
    end

    begin
      case ch
      when '-'
        inner = map.at(in_dir == :down ? r + 1 : r - 1, c)
        inside_ids.add(inner) if ids.include?(inner)
      when '|'
        inner = map.at(r, in_dir == :right ? c + 1 : c - 1)
        inside_ids.add(inner) if ids.include?(inner)
      when 'F'
        if %i[up left].include?(in_dir)
          [[r - 1, c - 1], [r - 1, c], [r, c - 1]].each do |ri, ci|
            inner = map.at(ri, ci)
            inside_ids.add(inner) if ids.include?(inner)
          end
        else
          inner = map.at(r + 1, c + 1)
          inside_ids.add(inner) if ids.include?(inner)
        end
        if dir == :left
          dir = :down
          in_dir = in_dir == :up ? :left : :right
        elsif dir == :up
          dir = :right
          in_dir = in_dir == :left ? :up : :down
        end
      when '7'
        if %i[up right].include?(in_dir)
          [[r - 1, c], [r - 1, c + 1], [r, c + 1]].each do |ri, ci|
            inner = map.at(ri, ci)
            inside_ids.add(inner) if ids.include?(inner)
          end
        else
          inner = map.at(r + 1, c - 1)
          inside_ids.add(inner) if ids.include?(inner)
        end
        if dir == :right
          dir = :down
          in_dir = in_dir == :up ? :right : :left
        elsif dir == :up
          dir = :left
          in_dir = in_dir == :right ? :up : :down
        end
      when 'J'
        if %i[right down].include?(in_dir)
          [[r, c + 1], [r + 1, c + 1], [r + 1, c]].each do |ri, ci|
            inner = map.at(ri, ci)
            inside_ids.add(inner) if ids.include?(inner)
          end
        else
          inner = map.at(r - 1, c - 1)
          inside_ids.add(inner) if ids.include?(inner)
        end
        if dir == :down
          dir = :left
          in_dir = in_dir == :right ? :down : :up
        elsif dir == :right
          dir = :up
          in_dir = in_dir == :down ? :right : :left
        end
      when 'L'
        if %i[left down].include?(in_dir)
          [[r, c - 1], [r + 1, c - 1], [r + 1, c]].each do |ri, ci|
            inner = map.at(ri, ci)
            inside_ids.add(inner) if ids.include?(inner)
          end
        else
          inner = map.at(r - 1, c + 1)
          inside_ids.add(inner) if ids.include?(inner)
        end
        if dir == :down
          dir = :right
          in_dir = in_dir == :left ? :down : :up
        elsif dir == :left
          dir = :up
          in_dir = in_dir == :down ? :left : :right
        end
      end

      case dir
      when :left
        c -= 1
      when :right
        c += 1
      when :up
        r -= 1
      when :down
        r += 1
      end
      ch = map.at(r, c)
    end while ch != 'S'

    count = 0
    map.each { |_, _, ch| count += 1 if inside_ids.include?(ch) }
    count
  end

  def flood_fill(map, loop, ri, ci, ch)
    q = []
    q << [ri, ci]
    until q.empty?
      ri, ci = q.shift
      next unless map.at(ri, ci) == '.'

      map.set(ri, ci, ch)
      [[ri - 1, ci], [ri + 1, ci], [ri, ci - 1], [ri, ci + 1]].each do |r, c|
        q << [r, c] if map.at(r, c) == '.'
      end
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
