encode :: Eq a => [a] -> [(a,Int)]
encode [] = []
encode (x:xs) = (x,n) : encode rest
  where
    n = length $ fst encode_do
    rest = snd encode_do
    encode_do = span (==x) (x:xs)
