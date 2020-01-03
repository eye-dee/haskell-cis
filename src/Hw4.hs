module Hw4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun11 :: [Integer] -> Integer
fun11 x = product (map (\i -> i - 2) (filter even x))

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

makeList :: Integer -> Integer
makeList n
    | even n = div n 2
    | otherwise = 3 * n + 1

fun22 :: Integer -> Integer
fun22 n = sum (filter even (takeWhile (>1) (iterate makeList n)))
