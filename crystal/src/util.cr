class Util
  def self.data_file_lines(year : Int32, day : Int32, part_num : Int32, testing : Bool) : Array(String)
    day_str = sprintf("%02d", day)
    testing_str = testing ? "_test" : ""
    path = "../data/y#{year}/day#{day_str}_#{part_num}#{testing_str}.txt"
    lines = File.read_lines(path)
    lines.delete("")
    lines
  end
end
