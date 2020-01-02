{-# OPTIONS_GHC -Wall #-}
module LogAnalysis
    (
        parse, insert, buildLogMessageTree, inOrder
    ) where
import Log

parseMessage :: String -> LogMessage
parseMessage x = parseMessageFromArray (words x)

parseMessageFromArray :: [String] -> LogMessage
parseMessageFromArray("I": xs) = LogMessage Info (convertToInt (head xs)) (unwords (drop 1 xs))
parseMessageFromArray("W": xs) = LogMessage Warning (convertToInt (head xs)) (unwords (drop 1 xs))
parseMessageFromArray("E": xs) =
    LogMessage (Error (convertToInt (head xs))) (convertToInt (head (drop 1 xs))) (unwords (drop 2 xs))
parseMessageFromArray [] = Unknown "empty"
parseMessageFromArray x = Unknown (unwords x)

convertToInt :: String -> Int
convertToInt x = read x :: Int

parse :: String -> [LogMessage]
parse x = parseAsArray (lines x)

parseAsArray :: [String] -> [LogMessage]
parseAsArray [] = []
parseAsArray (x:xs) = (parseMessage x) : (parseAsArray xs)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) y = y
insert x Leaf = Node Leaf x Leaf
insert logMessage@(LogMessage _ value _) (Node leftNode centerNode@(LogMessage _ centerNodeValue _) rightNode)
    | value < centerNodeValue = Node (insert logMessage leftNode) centerNode rightNode
    | otherwise = Node leftNode centerNode (insert logMessage rightNode)

buildLogMessageTree :: [LogMessage] -> MessageTree
buildLogMessageTree [] = Leaf
buildLogMessageTree (x:xs) = insert x (buildLogMessageTree xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left center right) = (inOrder left) ++ center:(inOrder right)
