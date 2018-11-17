sort []=[]
sort (head:others) = (sort (filter (head <) others)) ++ [head] ++ (sort (filter (head >) others))
