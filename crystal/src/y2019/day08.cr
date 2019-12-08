require "../day"

module Year2019
  class Day08 < Day
    @@num_cols = 25
    @@num_rows = 6

    def part1
      if @testing
        raise "no tests for part 1"
      end
      layers = extract_layers(data_lines())

      flat_layers = layers.map(&.flatten)
      min_zeroes_layer = flat_layers.min_by { |layer| layer.count(0) }
      val = min_zeroes_layer.count { |pixel| pixel == 1 } *
            min_zeroes_layer.count { |pixel| pixel == 2 }
      puts(val)
    end

    # Assumes all final pixels are either black or white, never transparent.
    # Also, we invert colors: white pixels are printed. The puzzle author
    # must have assumed everybody uses dark mode.
    def part2
      if @testing
        raise "no tests for part 2"
      end
      layers = extract_layers(data_lines(part_number: 1))

      final_image_rows : Array(String) = [] of String
      @@num_rows.times do |row|
        final_image_rows << String.build do |row_str|
          @@num_cols.times do |col|
            layers.each do |layer|
              pixel = layer[row][col]
              case pixel
              when 0
                row_str << "  "
                break
              when 1
                row_str << "* "
                break
              end
            end
          end
        end
      end
      puts(final_image_rows.join("\n"))
    end

    def extract_layers(lines)
      pixels = lines[0].split("").map(&.to_i)
      rows = pixels.in_groups_of(@@num_cols, 0)
      rows.in_groups_of(@@num_rows, [] of Int32)
    end
  end
end

AoC.register(Year2019::Day08)
