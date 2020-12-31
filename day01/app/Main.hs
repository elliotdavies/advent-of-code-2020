{-# LANGUAGE
      DataKinds,
      NoImplicitPrelude,
      NoStarIsType,
      PolyKinds,
      TypeFamilies, 
      TypeOperators,
      UndecidableInstances #-}

module Main where

import Data.Proxy (Proxy(..))
import GHC.TypeNats
import Prelude (IO, Ordering(..), undefined)

type NatPair = (Nat, Nat)

-- Append lists
type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- map (x,) xs
type family Tuples (x :: k) (xs :: [k']) :: [(k, k')] where
  Tuples x '[] = '[]
  Tuples x (y ': ys) = '(x, y) ': Tuples x ys

type family UniquePairs (xs :: [k]) :: [NatPair] where
  UniquePairs (x ': xs) = (Tuples x xs) ++ (UniquePairs xs)

type family PairSum (p :: NatPair) :: Nat where
  PairSum '(x, y) = x + y

type family PairMult (p :: NatPair) :: Nat where
  PairMult '(x, y) = x * y

type family Sums (ps :: [NatPair]) :: [Nat] where
  Sums (p ': ps) = (PairSum p) ': (Sums ps)

type family FindPair_ (p :: NatPair) (ps :: [NatPair]) (o :: Ordering) :: NatPair where
  FindPair_ p ps 'EQ = p
  FindPair_ p ps _   = FindPair ps

type family FindPair (ps :: [NatPair]) :: NatPair where
  FindPair (p ': ps) = FindPair_ p ps (CmpNat (PairSum p) 2020)

-- Sample input
type Inputs =
  '[ 1721
   , 979
   , 366
   , 299
   , 675
   , 1456
   ]

-- `:type result` in GHCi
result ::
  ( pairs  ~ UniquePairs Inputs
  , pair   ~ FindPair pairs
  , result ~ PairMult pair
  )
  => Proxy result
result = Proxy

main :: IO ()
main = undefined
