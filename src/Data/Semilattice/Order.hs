{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Semilattice.Order where

import Data.Semilattice.Join
import Data.Semilattice.Lower
import Data.Semilattice.Meet
import Data.Semilattice.Upper

-- | A 'Join'- and 'Meet'-semilattice for any total 'Ord'ering.
newtype Order a = Order { getOrder :: a }
  deriving (Bounded, Enum, Eq, Foldable, Functor, Lower, Num, Ord, Read, Show, Traversable, Upper)

instance Ord a => Join (Order a) where
  a \/ b
    | compare a b == LT = b
    | otherwise         = a

instance Ord a => Meet (Order a) where
  a /\ b
    | compare a b == LT = a
    | otherwise         = b
