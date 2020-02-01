module Lib
    ( someFunc
    ) where
import LogAnalysis

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
validateInternal :: Integer -> Bool
validateInternal n = mod n 10 == 0

validate :: Integer -> Bool
validate n = validateInternal (sumDigits (doubleEveryOther (toDigits n)))

someFunc :: IO ()
someFunc = do
    print (toDigits 4012888888881881)
--    print (sumDigits (doubleEveryOther (toDigits 0)))
--    print (sumDigits (doubleEveryOther (toDigits 1)))
--    print (sumDigits (doubleEveryOther (toDigits 4012888888881881)))
--    print (sumDigits (doubleEveryOther (toDigits 4012888888881881123)))
--    print (sumDigits (doubleEveryOther (toDigits 401288888888188112312314)))
--    print (sumDigits (doubleEveryOther (toDigits 401288888888188112312314123134123)))
--    print (sumDigits (doubleEveryOther (toDigits 40128888888818811231231412313412312314234)))
--    print (sumDigits (doubleEveryOther (toDigits 40128888888818811231231412313412312314234123123123)))
--    print (sumDigits (doubleEveryOther (toDigits 4012888888881881123123141231341231231423412312312323123123)))
--    print (sumDigits (doubleEveryOther (toDigits 0418234716239847162983765189724618297364182736491872364897123649871234)))
--    print (sumDigits (doubleEveryOther (toDigits 2348576238452834758923745982374598237459827345987234985792384579283475)))
--    print (sumDigits (doubleEveryOther (toDigits 47952873495827348957623895762387459234587239845238452834659827645287364952645782634985762398475623874659287346582736452)))
