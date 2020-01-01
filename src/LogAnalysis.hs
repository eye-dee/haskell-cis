{-# OPTIONS_GHC -Wall #-}
module LogAnalysis
    ( parse
    ) where
import Log

parseMessage :: String -> LogMessage
parseMessage x = parseMessageFromArray (words x)

parseMessageFromArray :: [String] -> LogMessage
parseMessageFromArray("I": xs) = LogMessage Info (convertToInt (head (drop 1 xs))) (unwords (drop 2 xs))
parseMessageFromArray("W": xs) = LogMessage Warning (convertToInt (head (drop 1 xs))) (unwords (drop 2 xs))
parseMessageFromArray("E": xs) =
    LogMessage (Error (convertToInt (head (drop 1 xs)))) (convertToInt (head (drop 2 xs))) (unwords (drop 2 xs))
parseMessageFromArray [] = Unknown "empty"
parseMessageFromArray x = Unknown (unwords x)

convertToInt :: String -> Int
convertToInt x = read x :: Int

parse :: String -> [LogMessage]
parse x = parseAsArray (lines x)

parseAsArray :: [String] -> [LogMessage]
parseAsArray [] = []
parseAsArray (x:xs) = (parseMessage x) : (parseAsArray xs)
