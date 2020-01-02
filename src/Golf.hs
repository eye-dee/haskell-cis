module Golf where

skips :: [a] -> [[a]]
skips [] = []
skips x = skipsInternal [1..length x] (zip [1..] x)

filterDividedBy :: Int -> (Int, a) -> Bool
filterDividedBy x (y, _) = (mod y x) == 0

skipsInternal :: [Int] -> [(Int, a)] -> [[a]]
skipsInternal [] _ = []
skipsInternal (i:is) (x) = (map snd (filter (filterDividedBy i) x)) : (skipsInternal is x)
