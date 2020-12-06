# Custom Customs

require 'set'

class Day06 < Day
  def part1
    entries = data_lines(1, skip_empty_lines=false)
    groups = group_entries(entries)
    puts(groups.map { |group| num_yesses_or(group) }.reduce(:+))
  end

  def part2
    entries = data_lines(1, skip_empty_lines=false)
    groups = group_entries(entries)
    puts(groups.map { |group| num_yesses_and(group) }.reduce(:+))
  end

  def num_yesses_or(group)
    unique_chars = Set.new(group.join('').chars)
    unique_chars.length
  end

  def num_yesses_and(group)
    sets = group.map { |answers| Set.new(answers.chars) }
    common_chars = sets.reduce(:intersection)
    common_chars.length
  end

  def group_entries(entries)
    groups = []
    group = []
    groups << group
    entries.each do |entry|
      if entry.empty?
        group = []
        groups << group
      else
        group << entry
      end
    end
    groups
  end
end
