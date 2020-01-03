module Golf where

import Data.List
import Data.Char

skips :: [a] -> [[a]]
skips [] = []
skips x = skipsInternal [1..length x] (zip [1..] x)

filterDividedBy :: Int -> (Int, a) -> Bool
filterDividedBy x (y, _) = (mod y x) == 0

skipsInternal :: [Int] -> [(Int, a)] -> [[a]]
skipsInternal [] _ = []
skipsInternal (i:is) (x) = (map snd (filter (filterDividedBy i) x)) : (skipsInternal is x)

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [x] = []
localMaxima [x,y] = []
localMaxima (x:rest@(y:z:xs))
    | x < y && y > z = y:localMaxima(rest)
    | otherwise = localMaxima(rest)

histogram :: [Integer] -> String
histogram xs = let counts = map (length . (flip filter $ xs) . (==)) [0..9]
                   height = foldl1 max counts
                   lines = map (\x -> map (display . (>= x)) counts) [height, height - 1 .. 1]
               in unlines $ lines ++ [replicate 10 '='] ++ [concatMap show [0..9]]

display :: Bool -> Char
display True = '*'
display False = ' '
--histogram :: [Int] -> String
--histogram x = (intercalate "\n" (reverse (transpose (histogramToTable x)))) ++ "\n"
--
--histogramToTable :: [Int] -> [String]
--histogramToTable x = histogramToTableInternal [0..9] x
--
--histogramToTableInternal :: [Int] -> [Int] -> [String]
--histogramToTableInternal [] _ = []
--histogramToTableInternal (x:xs) y = (histogramToTableWithIndex x y):(histogramToTableInternal xs y)
--
--histogramToTableWithIndex :: Int -> [Int] -> String
--histogramToTableWithIndex i x =
--    [intToDigit i] ++ "=" ++ (take (length (filter (i==) x)) (repeat (intToDigit i))) ++
--    (take (10 - length (filter (i==) x)) (repeat ' '))
