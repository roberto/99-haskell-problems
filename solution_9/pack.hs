pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = pack_do [[x]] xs

pack_do :: Eq a => [[a]] -> [a] -> [[a]] 
pack_do acc [] = acc
pack_do acc (x:xs)
 | (last (last acc)) == x = pack_do (init acc ++ [(last acc) ++ [x]]) xs
 | otherwise = pack_do (acc ++ [[x]]) xs


pack' :: Eq a => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = result
  where
    pack_do' cur acc [] = acc
    pack_do' cur acc (y:ys)
      | cur == y = pack_do' cur (init acc ++ [(last acc) ++ [cur]]) ys
      | otherwise = pack_do' y (acc ++ [[y]]) ys
    result = pack_do' x [[x]] xs

pack'' :: Eq a => [a] -> [[a]]
pack'' [] = []
pack'' (x:xs) = h ++ t
  where
    h = [fst pack_do'']
    t = pack'' (snd pack_do'')
    pack_do'' = span (==x) (x:xs)

pack''' :: Eq a => [a] -> [[a]]
pack''' [] = []
pack''' (x:xs) = (x:h) : pack''' t
  where
    (h,t) = span (==x) xs
