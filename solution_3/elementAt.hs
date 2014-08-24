elementAt :: [a] -> Integer -> a
elementAt [] _ = error "out of bounds"
elementAt list 1 = head list
elementAt (_:xs) index = elementAt xs (index - 1)
