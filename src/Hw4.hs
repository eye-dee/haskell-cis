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

data Tree a =
    Leaf | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

insertNode :: a -> Tree a -> Tree a
insertNode x Leaf = Node 0 Leaf x Leaf
insertNode x (Node height leftTree node rightTree)
    | treeHeight leftTree < treeHeight rightTree = Node height (insertNode x leftTree) node rightTree
    | treeHeight leftTree > treeHeight rightTree = Node height leftTree node (insertNode x rightTree)
    | otherwise = Node (1 + treeHeight (insertNode x rightTree)) leftTree node (insertNode x rightTree)

treeHeight :: Tree a -> Integer
treeHeight Leaf = 0
treeHeight (Node h _ _ _) = h

-- Pretty printing tree
prettyTree :: (Show a) => Tree a -> String
prettyTree Leaf = ""
prettyTree (Node h l x r) = prettyPrint((Node h l x r),0)

prettyPrint :: (Show a) => (Tree a, Int)-> String
prettyPrint (Leaf,_) = ""
prettyPrint ((Node h l x r),z) = prettyPrint(r,z+6) ++ replicate z ' '
                                 ++ show x ++ "_" ++ show h ++ ['\n'] ++ prettyPrint(l,z+6)

fibonacci :: Integer -> Integer
fibonacci n
    | n >= 0 = fibonacciInternal 0 1 n
    | n < 0 = fibonacciInternalNegative 0 1 n

fibonacciInternalNegative :: Integer -> Integer -> Integer -> Integer
fibonacciInternalNegative a _ 0 = a
fibonacciInternalNegative _ b (-1) = b
fibonacciInternalNegative a b n = fibonacciInternalNegative b (a - b) (n + 1)

fibonacciInternal :: Integer -> Integer -> Integer -> Integer
fibonacciInternal a _ 0 = a
fibonacciInternal _ b 1 = b
fibonacciInternal a b n = fibonacciInternal b (b + a) (n - 1)

seqA :: Integer -> Integer
seqA n = let
        helper a b c 0 = a
        helper a b c 1 = b
        helper a b c 2 = c
        helper a b c n = helper b c (b + c - 2*a) (n - 1)
      in helper 1 2 3 n

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0, 1)
sum'n'count x = (sum x, count x)
    where
        sum 0 = 0
        sum x
            | x > 0 = mod x 10 + (sum $ div x 10)
            | x < 0 = sum (-x)
        count :: Integer -> Integer
        count 0 = 0
        count x
            | x > 0 = 1 + (count $ div x 10)
            | x < 0 = count (-x)


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b
    | a < b = integrationRun f a b $ step a b
    | otherwise = integrationRun f a b (-(step b a))
    where
        step :: Double -> Double -> Double
        step a b = (b - a) / 1000.0
        helper :: (Double -> Double) -> Double -> Double -> Double
        helper f a b = ((f a) + (f b)) / 2 * (b - a)
        integrationRun :: (Double -> Double) -> Double -> Double -> Double -> Double
        integrationRun f a b h
            | h < 0 && a + h > b = helper f a (a + h) + integrationRun f (a + h) b h
            | h > 0 && a + h < b = helper f a (a + h) + integrationRun f (a + h) b h
            | otherwise = helper f a b
--ghci
--xor :: [Bool] -> Bool
--xor = foldr
