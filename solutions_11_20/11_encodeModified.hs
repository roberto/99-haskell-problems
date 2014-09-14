data Element a = Multiple Int a | Single a deriving Show

encodeModified :: Eq a => [a] -> [Element a]
encodeModified [] = []
encodeModified (x:xs) = (encode(collected) : encodeModified(rest)) where
  (collected, rest) = span (==x) (x:xs)
  encode [y] = Single y
  encode l = Multiple (length l) x
