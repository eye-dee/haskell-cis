module Hw4 where

import Data.Char
import Data.List


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

class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString True = "true"
    toString False = "false"

instance Printable () where
    toString = \_ -> "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
    toString (a, b) = "(" ++ (toString a) ++ "," ++ (toString b) ++ ")"

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a
        | (doesEnrageMork a) && (doesEnrageGork a) = stomp (stab a)
        | doesEnrageMork a = stomp a
        | doesEnrageGork a = stab a
        | otherwise = a


class (Bounded a, Enum a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x
    | x == maxBound = minBound
    | otherwise = succ x

  spred :: a -> a
  spred x
    | x == minBound = maxBound
    | otherwise = pred x

instance SafeEnum Bool
--ghci
--xor :: [Bool] -> Bool
--xor = foldr


foo 0 x = x
foo n x = let xx = foo (n - 1) (x + 1)
          in seq xx xx

bar 0 f = f
bar x f = let ff = \a -> f (x + a)
              xx = x - 1
          in ff `seq` xx `seq` bar xx ff

baz 0 (x, y) = x + y
baz n (x, y) = let x' = x + 1
                   y' = y - 1
                   p  = (x', y')
                   n' = n - 1
               in p `seq` n' `seq` baz n' p

quux 0 (x, y) = x + y
quux n (x, y) = let x' = x + 1
                    y' = y - 1
                    p  = (x', y')
                    n' = n - 1
                in x' `seq` y' `seq` n' `seq` quux n' p


oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs)
    | x `mod` 2 == 0 = oddsOnly xs
    | x `mod` 2 == 1 = x : oddsOnly xs
    | otherwise = x : oddsOnly xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome a = isPalindromeIn a (rev a)
    where
        isPalindromeIn [] [] = True
        isPalindromeIn [_] [] = False
        isPalindromeIn [] [_] = False
        isPalindromeIn (x:xs) (y:ys) = (y == x) && isPalindromeIn xs ys
        rev a = revIn a []
        revIn [] a = a
        revIn (x:xs) a = revIn xs (x:a)

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 x y z = sum2 z (sum2 x y)
sum2 :: Num a => [a] -> [a] -> [a]
sum2 [] [] = []
sum2 a [] = a
sum2 [] a = a
sum2 (x:xs) (y:ys) = (x+y) : sum2 xs ys

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) = reverse (groupElemsIn [x] xs [])
    where
        groupElemsIn [] [] acc = acc
        groupElemsIn x [] acc = x:acc
        groupElemsIn cur (x:xs) acc
            | head cur == x = groupElemsIn (x:cur) xs acc
            | otherwise = groupElemsIn (x : []) xs (cur:acc)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [a] = [a]
qsort x@(y:xs)= qsort (filter (<= y) xs) ++ y : qsort (filter (> y) xs)


squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes [] = []
squares'n'cubes (x:xs) = x^2 : x^3 : squares'n'cubes xs

perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms (x:xs) = concatMap (insertElem x) (perms xs)
    where
        insertElem x [] = [[x]]
        insertElem x yss@(y:ys) = (x:yss) : map (y:) (insertElem x ys)


isAnyUpper :: String -> Bool
isAnyUpper = not . all isUpper

delAllUpper :: String -> String
delAllUpper = unwords . (filter isAnyUpper) . words

max33 :: Ord a => a -> a -> a -> a
max33 a b c
   | a >= b && a >= c = a
   | b >= a && b >= c = b
   | c >= a && c >= b = c

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 max33

data Odd = Odd Integer deriving (Eq,Show)
instance Enum Odd where
    pred (Odd x) = Odd (x - 2)
    succ (Odd x) = Odd (x + 2)
    toEnum x = Odd $ toInteger x
    fromEnum (Odd x) = fromInteger x
    enumFrom (Odd x) = map (Odd) [x,x+2..]
    enumFromThen (Odd x) (Odd y) = map (Odd) [x,y..]
    enumFromTo (Odd x) (Odd y) = map (Odd) [x,x+2..y]
    enumFromThenTo (Odd x) (Odd y) (Odd z) = map (Odd) [x,y..z]

coins ::(Ord a, Num a) => [a]
coins = [2,3,7]

change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change amount = [ c:cs | c <- coins, amount >= c, cs <- change (amount - c) ]

sumOdd :: [Integer] -> Integer
sumOdd = foldr f 0
    where
        f x y
            | mod x 2 == 1 = x + y
            | otherwise = y

meanList :: [Double] -> Double
meanList = uncurry (\x y -> x / y) . foldr (\x (f,s) -> (x + f, s + 1)) (0,0)
--
--evenOnly :: [a] -> [a]
--evenOnly x = fst (foldr fold ([], (length x)) x)
--    where
--        fold :: a -> ([a], Int) -> ([a], Int)
--        fold x (xs, i)
--            | (mod i 2) == 0 = (x:xs, i - 1)
--            | otherwise = (xs, i - 1)

evenOnly :: [a] -> [a]
evenOnly x = evens x 1
    where
        evens [] _ = []
        evens (x:xs) i
            | even i = x : evens xs (i + 1)
            | otherwise = evens xs (i + 1)

--evenOnly :: [a] -> [a]
--evenOnly = fst . foldl fold ([], 1)
--    where
--        fold :: ([a], Int) -> a -> ([a], Int)
--        fold (xs, i) x
--            | (mod i 2) == 0 = ((seq xs (xs ++ [x])), i + 1)
--            | otherwise = (xs, i + 1)


revRange :: (Char,Char) -> [Char]
revRange = unfoldr g
  where
    g :: (Char, Char) -> Maybe (Char, (Char, Char))
    g (p1, p2)
        | p1 <= p2 = Just (p2, (p1, (pred p2)))
        | otherwise = Nothing

data Bit = Zero | One deriving (Show)
data Sign = Minus | Plus deriving (Show)
data Z = Z Sign [Bit] deriving (Show)

bitToInt :: Z -> Int
bitToInt (Z y x) =  (sign y) * (fst (foldl (\(sum, mul) x -> (sum + x*mul, mul*2)) (0, 1) (map toBit x)))

intToBit :: Int -> Z
intToBit x = Z (signn (signum x)) (unfoldr ops (abs x))
    where
        ops x
            | x == 0 = Nothing
            | mod x 2 == 0 = Just (Zero, (div x 2))
            | otherwise = Just (One, (div x 2))

add :: Z -> Z -> Z
add x y = intToBit ((bitToInt x) + (bitToInt y))

toBit :: Bit -> Int
toBit Zero = 0
toBit One = 1

sign Minus = (-1)
sign Plus = 1
signn (-1) = Minus
signn 1 = Plus
signn 0 = Plus

mul :: Z -> Z -> Z
mul x y = intToBit ((bitToInt x) * (bitToInt y))


fooo 0 = undefined
fooo x = x : fooo (x - 1)


data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

parsePerson :: String -> Either Error Person
parsePerson x = parsePersonFromList (words $ filter (/=' ') x)

parsePersonFromList :: [String] -> Either Error Person
parsePersonFromList l@(x:y:z:xs)
    | not (all isDigit $ dropWhileEqual z) = Left (IncorrectDataError $ dropWhileEqual z)
    | any (\x -> (find (\y -> y == '=') x) == Nothing) l = Left ParsingError
    | otherwise = Right Person
        {firstName = dropWhileEqual x, lastName = dropWhileEqual y, age = read (dropWhileEqual z):: Int}
parsePersonFromList _ = Left IncompleteDataError

dropWhileEqual = tail . dropWhile (/='=')
