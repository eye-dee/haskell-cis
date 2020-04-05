module Main where

import Lib
import DataTypes
import LogAnalysisMain
import Golf
import Hw5
import Hw7
import Data.List
import System.Directory
import Data.Monoid
import Control.Monad (ap, liftM)


onlyPattern :: String -> [String] -> IO [String]
onlyPattern x y = return (filter (isInfixOf x) y)

removeEach :: [String] -> IO ()
removeEach [] = return ()
removeEach (x:xs) = do
    putStrLn $ "Removing: " ++ x
    removeFile x
    removeEach xs

execute :: String -> IO ()
execute pattern = do
    dirs <- getDirectoryContents "temp"
    filtered <- (onlyPattern pattern dirs)
    removeEach filtered

main :: IO ()
main = do
    putStrLn $ foldl (++) ("") (items shopping1)
    putStrLn $ show (total shopping1)

main1 :: IO ()
main1 = do
    putStrLn "Substring: "
    pattern <- getLine
    if pattern == [] then putStrLn "Canceled" else (execute pattern)


newtype Writer w a = Writer {runWriter :: (a, w)} deriving Show

writer :: (a, w) -> Writer w a
writer = Writer

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  m >>= k =
    let (x, u) = runWriter m
        (y, v) = runWriter $ k x
    in Writer (y, u `mappend` v)

data MyMonoid = MyMonoid (Integer, [String])

instance Semigroup MyMonoid where
    MyMonoid (x1, y1) <> MyMonoid (x2, y2) = MyMonoid(x1 + x2, y1 ++ y2)

instance Monoid MyMonoid where
    mempty = MyMonoid (0, [])
    mappend (MyMonoid (x1, y1)) (MyMonoid (x2, y2)) = MyMonoid (x1 + x2, y1 ++ y2)

instance (Monoid w) => Applicative (Writer w) where
  pure  = return
  (<*>) = ap

instance (Monoid w) => Functor (Writer w) where
  fmap = liftM

type Shopping = Writer MyMonoid ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328

purchase :: String -> Integer -> Shopping
purchase item cost = writer ((), MyMonoid (cost, [item]))

total :: Shopping -> Integer
total x = let (MyMonoid (t, _)) = execWriter x in t

items :: Shopping -> [String]
items x = let (MyMonoid (_, l)) = execWriter x in l
