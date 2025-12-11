#!/usr/bin/env ruby
#
# Reactor

require_relative '../day'

class Day11 < Day
  def do_part1(lines)
    conns = parse_connections(lines)
    num_paths_you_to_out(conns)
  end

  def do_part2(lines)
    conns = parse_connections(lines)
    num_paths_through_dac_and_fft(conns)
  end

  private

  def num_paths_you_to_out(conns)
    clear_cache
    num_paths_from_to(:you, :out, conns)
  end

  # We first count the number of paths from dac->fft or fft->dac (one of
  # those will be zero). That tells us which needs to come first. Then we
  # count the paths from :svr (the start) to that, and from the other to
  # :out. We have three path counts. Multiply them, and that's our answer.
  def num_paths_through_dac_and_fft(conns)
    clear_cache
    # one of these two, dac->fft or fft->dac, will be zero
    dac_to_fft = num_paths_from_to(:dac, :fft, conns)
    fft_to_dac = if dac_to_fft == 0
                   clear_cache
                   num_paths_from_to(:fft, :dac, conns)
                 else
                   0
                 end
    if dac_to_fft > 0
      clear_cache
      before = num_paths_from_to(:svr, :dac, conns)
      clear_cache
      after = num_paths_from_to(:fft, :out, conns)
      before * dac_to_fft * after
    else
      clear_cache
      before = num_paths_from_to(:svr, :fft, conns)
      clear_cache
      after = num_paths_from_to(:dac, :out, conns)
      before * fft_to_dac * after
    end
  end

  # Returns number of paths from `from` to :out. Uses `@cache`. Assumes no
  # cycles.
  def num_paths_from_to(from, to, conns)
    return 1 if from == to
    return 0 if from == :out && to != :out

    children = conns[from]
    return 1 if children.nil?

    children.map do |child|
      cached = @cache[[child, to]]
      if cached.nil?
        cached = num_paths_from_to(child, to, conns)
        @cache[[child, to]] = cached
      end
      cached
    end.sum
  end

  def parse_connections(lines)
    conns = {}
    lines.map do |line|
      node, rest = line.split(': ')
      outputs = rest.split(' ')
      conns[node.to_sym] = outputs.map(&:to_sym)
    end
    conns
  end

  def clear_cache
    @cache = {}
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
