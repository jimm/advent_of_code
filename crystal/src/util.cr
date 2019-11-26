class Util
  def self.data_file_lines(year: Int32, day: Int32, part_num: Int32, testing: Bool)
    fname = "day"
    fname += "_test" if testing
    fname += ".txt"
    path = "data/y#{year}/day#{%02d % day}#{testing ? '_test' : ''}"
    File.read_lines(path)
  end
end
