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
    debug("\n\n\n++++++++++++++++++++++++++++++++")
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

  # def count_locs_inside_loop(map, loop, start_loc, start_char)
  #   debug("map =\n#{map}")
  #   init_non_loop_chars(map, loop)
  #   map.set(start_loc[0], start_loc[1], start_char) # avoids 'S' special case

  #   debug("map =\n#{map}")
  #   num_inside = 0
  #   map.height.times do |row_idx|
  #     num_border_crossings = 0
  #     debug("\nrow = #{map.row_cells(row_idx).join}")
  #     prev_ch = '.'
  #     map.width.times do |col_idx|
  #       ch = map.at(row_idx, col_idx)
  #       case ch
  #       when '.'
  #         num_inside += 1 if num_border_crossings.odd? && prev_ch == '.'
  #         map.set(row_idx, col_idx, num_border_crossings.odd? ? 'I' : 'O') # DEBUG
  #         debug(". at [#{row_idx}, #{col_idx}], num_border_crossings = #{num_border_crossings}, prev_ch = #{prev_ch}")
  #         prev_ch = '.'
  #       when '|'
  #         num_border_crossings += 1
  #         prev_ch = '.'
  #         debug("#{ch} at [#{row_idx}, #{col_idx}], num_border_crossings => #{num_border_crossings}")
  #       when '-'
  #         prev_ch = ch
  #       when 'F', 'L'
  #         num_border_crossings += 1
  #         debug("#{ch} at [#{row_idx}, #{col_idx}], num_border_crossings => #{num_border_crossings}")
  #         prev_ch = ch
  #       when '7', 'J'
  #         num_border_crossings += 1
  #         debug("#{ch} at [#{row_idx}, #{col_idx}], num_border_crossings => #{num_border_crossings}")
  #         prev_ch = ch
  #       end
  #     end
  #     debug("row = #{map.row_cells(row_idx).join}")
  #   end
  #   debug("\nreturning #{num_inside}, map =\n#{map}")
  #   num_inside
  # end

  # We assume that all non-loop chars are '.'.
  def count_locs_inside_loop(map, loop, start_loc, start_char)
    debug("map =\n#{map}")
    init_non_loop_chars(map, loop)
    id = 'a'
    loc = map.find('.')
    while loc
      flood_fill(map, loop, loc[0], loc[1], id)
      id = id.succ
      loc = map.find('.')
    end
    debug("flood-filled map =\n#{map}")
    ids = ('a'...id).to_a
    debug("ids = #{ids}")

    outside_ids = Set.new
    dir = :down
    out_dir = :left
    r, c = start_loc

    # set initial direction and out_dir as if we were coming in to the starting loc
    ch = start_char_at(map, [r, c])
    case ch
    when 'F'
      dir = :left
      out_dir = :up
    when '7'
      dir = :right
      out_dir = :up
    when 'J'
      dir = :right
      out_dir = :down
    when 'L'
      dir = :left
      out_dir = :down
    end

    begin
      case ch
      when '-'
        outer = map.at(out_dir == :up ? r - 1 : r + 1, c)
        outside_ids.add(outer) if ids.include?(outer)
      when '|'
        outer = map.at(r, out_dir == :left ? c - 1 : c + 1)
        outside_ids.add(outer) if ids.include?(outer)
      when 'F'
        if %i[up left].include?(out_dir)
          [[r - 1, c - 1], [r - 1, c], [r, c - 1]].each do |ri, ci|
            outer = map.at(ri, ci)
            outside_ids.add(outer) if ids.include?(outer)
          end
        else
          outer = map.at(r + 1, c + 1)
          outside_ids.add(outer) if ids.include?(outer)
        end
        if dir == :left
          dir = :down
          out_dir = out_dir == :up ? :left : :right
        elsif dir == :up
          dir = :right
          out_dir = out_dir == :left ? :up : :down
        end
      when '7'
        if %i[up right].include?(out_dir)
          [[r - 1, c], [r - 1, c + 1], [r, c + 1]].each do |ri, ci|
            outer = map.at(ri, ci)
            outside_ids.add(outer) if ids.include?(outer)
          end
        else
          outer = map.at(r + 1, c - 1)
          outside_ids.add(outer) if ids.include?(outer)
        end
        if dir == :right
          dir = :down
          out_dir = out_dir == :up ? :right : :left
        elsif dir == :up
          dir = :left
          out_dir = out_dir == :right ? :up : :down
        end
      when 'J'
        if %i[right down].include?(out_dir)
          [[r, c + 1], [r + 1, c + 1], [r + 1, c]].each do |ri, ci|
            outer = map.at(ri, ci)
            outside_ids.add(outer) if ids.include?(outer)
          end
        else
          outer = map.at(r - 1, c - 1)
          outside_ids.add(outer) if ids.include?(outer)
        end
        if dir == :down
          dir = :left
          out_dir = out_dir == :right ? :down : :up
        elsif dir == :right
          dir = :up
          out_dir = out_dir == :down ? :right : :left
        end
      when 'L'
        if %i[left down].include?(out_dir)
          [[r, c - 1], [r + 1, c - 1], [r + 1, c]].each do |ri, ci|
            outer = map.at(ri, ci)
            outside_ids.add(outer) if ids.include?(outer)
          end
        else
          outer = map.at(r - 1, c + 1)
          outside_ids.add(outer) if ids.include?(outer)
        end
        if dir == :down
          dir = :right
          out_dir = out_dir == :left ? :down : :up
        elsif dir == :left
          dir = :up
          out_dir = out_dir == :down ? :left : :right
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

    inside_ids = ids - outside_ids.to_a
    count = 0
    map.each { |_, _, ch| count += 1 if inside_ids.include?(ch) }

    warn "outside_ids = #{outside_ids}" # DEBUG
    # DEBUG
    map.each do |ri, ci, ch|
      if outside_ids.include?(ch)
        map.set(ri, ci, 'O')
      elsif inside_ids.include?(ch)
        map.set(ri, ci, 'I')
      end
    end
    debug(map)

    debug("returning count = #{count}")
    count
  end

  # def find_outside_loc(map)
  #   ci = map.row_cells(0).index('.')
  #   return [0, ci] if ci

  #   ci = map.row_cells[-1].index('.')
  #   return [map.height - 1, ci] if ci

  #   ri = map.col_cells(0).index('.')
  #   return [ri, 0] if ri

  #   ri = map.col_cells(map.width - 1).index('.')
  #   return [ri, map.width - 1] if ri

  #   nil
  # end

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
