# Passport Processing

class Day04 < Day
  SORTED_VALID_FIELDS = %w(byr cid ecl eyr hcl hgt iyr pid)

  def part1
    passport_data = read_passports()
    valid_passport_data = passport_data.select { |p| valid_passport_fields?(p) }
    puts(valid_passport_data.length)
  end

  def part2
    passport_data = read_passports()
    valid_passport_data = passport_data.select { |p| valid_passport?(p) }
    puts(valid_passport_data.length)
  end

  def valid_passport_fields?(data)
    data.keys.sort - ['cid'] == SORTED_VALID_FIELDS - ['cid']
  end

  def valid_passport?(data)
    return false unless valid_passport_fields?(data)
    data.keys.each do |key|
      val = data[key]
      case key
      when 'byr'
        return false unless valid_year?(val, 1920, 2002)
      when 'iyr'
        return false unless valid_year?(val, 2010, 2020)
      when 'eyr'
        return false unless valid_year?(val, 2020, 2030)
      when 'hgt'
        return false unless valid_height?(val)
      when 'hcl'
        return false unless (val =~ /^#[0-9a-f]{6}$/) == 0
      when 'ecl'
        return false unless %w(amb blu brn gry grn hzl oth).include?(val)
      when 'pid'
        return false unless (val =~ /^\d{9}$/) == 0
      end
    end
    true
  end

  def valid_year?(val, min, max)
    val.length == 4 && val.to_i >= min && val.to_i <= max
  end

  def valid_height?(val)
    val =~ /(\d+)(\w+)/
    num = $1.to_i
    units = $2
    case units
    when 'cm'
      return false unless num >= 150 && num <= 193
    when 'in'
      return false unless num >= 59 && num <= 76
    else
      return false
    end
    true
  end

  def read_passports
    entries = data_lines(1, skip_empty_lines=false)
    passport_data = []
    curr_passport_data = {}
    passport_data << curr_passport_data
    entries.each do |entry|
      if entry.empty?
        curr_passport_data = {}
        passport_data << curr_passport_data
        next
      end
      entry.split(' ').each do |field|
        key, val = field.split(':')
        curr_passport_data[key] = val
      end
    end
    if passport_data[-1] == {}
      passport_data.pop
    end

    passport_data
  end
end
