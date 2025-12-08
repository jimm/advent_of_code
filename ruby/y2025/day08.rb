#!/usr/bin/env ruby
#
# Playground

require_relative '../day'
require_relative '../point'

class Day08 < Day
  def do_part1(lines)
    coords = parse_3d(lines)
    coords_and_dists = pairs_and_distances_between(coords)
                       .sort_by! { |pad| pad[2] }
    create_circuits(coords_and_dists, @testing ? 10 : 1000, [])
      .sort_by(&:length)
      .reverse
      .take(3)
      .reduce(1) { |product_of_lengths, circuit| product_of_lengths * circuit.length }
  end

  def do_part2(lines)
    coords = parse_3d(lines)
    coords_and_dists = pairs_and_distances_between(coords)
                       .sort_by! { |pad| pad[2] }
    c0, c1 = create_one_circuit(coords_and_dists, coords.length, [])
    c0.x * c1.x
  end

  private

  # Recursively connects coords and add to the proper circuits, creating and
  # merging circuits when necessary. Returns the resulting circuits after
  # `num_iters`.
  def create_circuits(coords_and_dists, num_iters, circuits)
    return circuits if num_iters == 0

    c0, c1 = coords_and_dists[0][..1]
    circuits = add_coords_to_circuits(c0, c1, circuits)
    create_circuits(coords_and_dists[1..], num_iters - 1, circuits)
  end

  # Recursively connects coords and add to the proper circuits, creating and
  # merging circuits when necessary. When we finally create one circuit
  # containing all the coordinates, return the last two coords that made
  # that happen.
  def create_one_circuit(coords_and_dists, num_coords, circuits)
    c0, c1 = coords_and_dists[0][..1]
    circuits = add_coords_to_circuits(c0, c1, circuits)
    return [c0, c1] if circuits.length == 1 && circuits[0].size == num_coords

    create_one_circuit(coords_and_dists[1..], num_coords, circuits)
  end

  # Given two coordinates and all existing circuits, connect them and update
  # circuits, returning the new list of circuits.
  def add_coords_to_circuits(c0, c1, circuits)
    c0_circuit = circuits.detect { |circuit| circuit.include?(c0) }
    c1_circuit = circuits.detect { |circuit| circuit.include?(c1) }

    if c0_circuit.nil? && c1_circuit.nil? # create a new circuit
      circuits << Set.new([c0, c1])
    elsif c0_circuit.nil?       # add c0 to c1 circuit
      c1_circuit << c0
    elsif c1_circuit.nil?       # add c1 to c0 circuit
      c0_circuit << c1
    elsif c0_circuit == c1_circuit # do nothing
      # nop
    else # two diff circuits; merge them
      c0_circuit.merge(c1_circuit)
      circuits.delete(c1_circuit)
    end
    circuits
  end

  def parse_3d(lines)
    lines.map do |line|
      Point.new(*line.split(',').map(&:to_i))
    end
  end

  # Returns an array of [c0, c1, squared-distance].
  def pairs_and_distances_between(coords)
    coords.combination(2).map do |c0, c1|
      [c0, c1, c0.distance_squared(c1)]
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
