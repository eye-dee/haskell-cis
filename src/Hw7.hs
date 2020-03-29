module Hw7 where

import Prelude hiding (lookup)
import qualified Data.List as L
import Data.Char

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty = ArrowMap (\_ -> Nothing)
    lookup k (ArrowMap f) = f k
    insert k v (ArrowMap f) = ArrowMap (\x -> insertFun k v f x) where
        insertFun k v f x
            | k == x = Just v
            | otherwise = f x
    delete k (ArrowMap f) = ArrowMap (\x -> deleteFun k f x) where
        deleteFun k f x
            | k == x = Nothing
            | otherwise = f x
    fromList [] = empty
    fromList x = ArrowMap (\key -> L.lookup key x)

data Point3D a = Point3D a a a deriving Show

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)

instance Functor GeomPrimitive where
    fmap f (Point (Point3D a b c)) = Point $ Point3D (f a) (f b) (f c)
    fmap f (LineSegment (Point3D a1 b1 c1) (Point3D a2 b2 c2)) = LineSegment (Point3D (f a1) (f b1) (f c1)) (Point3D (f a2) (f b2) (f c2))


data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree where
    fmap g (Leaf x) = Leaf (fmap g x)
    fmap g (Branch tl v tr) = Branch (fmap g tl) (fmap g v) (fmap g tr)

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry k1 k2) where
    fmap g (Entry p x) = Entry p (g x)

instance Functor (Map k1 k2) where
    fmap g (Map x) = Map (map (fmap g) x)

data Log a = Log [String] a

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = \x -> Log [msg] (f x)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log ((msg (f x)) ++ (msg (g (val (f x))))) (val (g (val (f x)))) where
    msg (Log x _) = x
    val (Log _ x) = x

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log prev a) f =
    let (Log next b) = f a
    in Log (prev ++ next) b

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken str
  | all isDigit str = Just $ Number $ read str
  | str == "+" = Just Plus
  | str == "-" = Just Minus
  | str == ")" = Just RightBrace
  | str == "(" = Just LeftBrace
  | otherwise  = Nothing

tokenize :: String -> Maybe [Token]
tokenize = sequence . map asToken . words


pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = do
    a <- [1..x]
    b <- [1..x]
    c <- [1..x]
    if a < b then "1" else []
    if b < c then "1" else []
    if (a^2) + (b^2) == (c^2) then "1" else []
    return (a,b,c)
