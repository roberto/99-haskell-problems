myLength :: [a] -> Integer
myLength [] = 0
myLength [x] = 1
myLength (x:xs) = myLength [x] + myLength xs

myLength' :: [a] -> Integer
myLength' [] = 0
myLength' (_:xs) = 1 + myLength' xs
