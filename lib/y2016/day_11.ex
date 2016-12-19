defmodule Y2016.Day11 do
  @floors [[{:gen, :thulium}, {:chip, :thulium}, {:gen, :plutonium}, {:gen, :strontium}], # floor 1
           [{:gen, :plutonium}, {:chip, :strontium}], # floor 2
           [{:gen, :promethium}, {:chip, :promethium}, {:gen, :ruthenium}, {:chip, :ruthenium}], # floor 3
           []]                  # floor 4
end

# F4: .. .. .. .. .. .. .. .. .. ..
# F3: .. .. .. .. .. .. MG MM RG RM
# F2: .. .. .. PM .. SM .. .. .. ..
# F1: TG TM PG .. SG .. .. .. .. ..
#
# 15 is too low
# 24 is wrong
# 32 is too high
