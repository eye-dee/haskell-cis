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
