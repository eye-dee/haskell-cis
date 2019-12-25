module Lib
    ( someFunc
    ) where

reallyBig :: Integer
reallyBig = 2^(2^(2^(2^2)))

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
    | n < 0 = []
    | otherwise = toDigits(div n 10) ++ [(mod n 10)]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = (mod n 10):toDigits(div n 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ([]) = []
doubleEveryOther ([x]) = [x]
doubleEveryOther ([x, y]) = [x * 2, y]
doubleEveryOther (rest) = (doubleEveryOther (init (init rest))) ++ [(last (init rest)) * 2] ++ [last rest]

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (xs:x) = sum (toDigits xs) + sumDigits x

someFunc :: IO ()
someFunc = do
    print (toDigits 1234567890)
    print (doubleEveryOther (toDigits 1234567890))
    print (sumDigits (doubleEveryOther (toDigits 1234567890)))
