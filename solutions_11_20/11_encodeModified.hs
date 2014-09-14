data Element a = Multiple Int a | Single a deriving Show

encodeModified :: Eq a => [a] -> [Element a]
encodeModified [] = []
encodeModified (x:xs) = (encoded : encodeModified(rest)) where
  process = span (==x) (x:xs)
  collected = fst process
  rest = snd process
  encoded
    | length collected == 1 = Single x
    | otherwise = Multiple (length collected) x
