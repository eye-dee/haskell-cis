module Hw5 where

import Data.Char
import Data.List
import Prelude hiding (lookup)
import qualified Data.List as L


data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero x = x
add (Suc x) y = add x (Suc y)

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul (Suc x) y = add (mul x y) y

fac :: Nat -> Nat
fac Zero = Suc Zero
fac y@(Suc x) = mul y (fac x)


data Tree a = Leaf a | Node (Tree a) (Tree a)

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf x) = (x, 1)
    go (Node x y) = pairSum (go x) (go y)
    pairSum :: (Int, Int) -> (Int, Int) -> (Int, Int)
    pairSum (x, y) (z, t) = (x + z, y + t)


--newtype A a b = A b
--newtype A a = A
--newtype A a = A a a
--newtype A a = A a
--newtype A = A
--newtype A a b = A a b
--newtype A = A A A
--newtype A = A a
newtype A = A A
--newtype A a b = A a

--xor :: Bool -> Bool -> Bool
--xor False b = b
--xor True a = not a
--
--newtype Xor = Xor { getXor :: Bool }
--    deriving (Eq,Show)
--
--instance Monoid Xor where
--    mempty = Xor False
--    mappend (Xor a) (Xor b) = Xor (xor a b)

--
--newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
--    deriving (Eq,Show)
--
--instance Monoid a => Monoid (Maybe' a) where
--  mempty = Maybe' $ Just mempty
--  mappend (Maybe' Nothing) _ = Maybe' Nothing
--  mappend _ (Maybe' Nothing) = Maybe' Nothing
--  mappend (Maybe' a) (Maybe' b) = Maybe' (mappend a b)
