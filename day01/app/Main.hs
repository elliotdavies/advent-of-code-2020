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
import Prelude (IO, undefined)

-- Append lists
type family (xs :: [k]) ++ (ys :: [k]) where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- Lookup within lists
type family IndexOf (x :: k) (xs :: [k]) :: Nat where
  IndexOf x (x ': xs) = 0
  IndexOf x (y ': xs) = 1 + (IndexOf x xs)

-- Index into lists
type family (xs :: [k]) !! (n :: Nat) where
  (x ': xs) !! 0 = x
  (x ': xs) !! n = xs !! (n - 1)

-- map (x,) xs
type family Tuples (x :: k) (xs :: [k]) where
  Tuples x '[] = '[]
  Tuples x (y ': ys) = '(x, y) ': Tuples x ys

type family UniquePairs (xs :: [k]) :: [(Nat, Nat)] where
  UniquePairs (x ': xs) = (Tuples x xs) ++ (UniquePairs xs)

type family PairSum (p :: (Nat, Nat)) :: Nat where
  PairSum '(x, y) = x + y

type family PairMult (p :: (Nat, Nat)) :: Nat where
  PairMult '(x, y) = x * y

type family Sums (ps :: [(Nat, Nat)]) :: [Nat] where
  Sums (p ': ps) = (PairSum p) ': (Sums ps)

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
  ( pairs ~ UniquePairs Inputs
  , idx ~ IndexOf 2020 (Sums pairs)
  , pair ~ (pairs !! idx)
  , result ~ PairMult pair
  )
  => Proxy result
result = Proxy

main :: IO ()
main = undefined
