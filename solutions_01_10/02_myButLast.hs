myButLast :: [a] -> a
myButLast (x:_:[]) = x 
myButLast (_:xs) = myButLast xs 
myButLast _ = error "List should have at least 2 elements"

myButLast' :: [a] -> Maybe a
myButLast' (x:_:[]) = Just x 
myButLast' (_:xs) = myButLast' xs 
myButLast' _ = Nothing
