module Enumerable
  # Divides `self` into arrays based on a delimiter, returning an array of
  # arrays of elements. Must either specfiy `func_sym` or pass in a block to
  # be applied to each element.
  def split(func_sym=nil)
    chunk { |entry| func_sym ? entry.send(func_sym) : yield(entry) }
      .reject { |delim_true, _| delim_true }
      .map { |_, vals| vals }
  end
end
