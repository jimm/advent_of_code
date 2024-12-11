(defn add
  "Add items to a set and return the set."
  [s & items]
  (each item items
    (put s item item))
  s)

(defn new
  "Create a new set that contains items and returns it."
  [& items]
  (add (table/new (length items)) ;items))
