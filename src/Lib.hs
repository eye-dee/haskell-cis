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

someFunc :: IO ()
someFunc = print ((toDigits 12345678910) ++ (toDigitsRev 12345678910))
