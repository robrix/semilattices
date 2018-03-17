{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Semilattice.Order where

import Data.Semilattice.Lower
import Data.Semilattice.Upper

-- | A 'Join'- and 'Meet'-semilattice for any total 'Ord'ering.
newtype Order a = Order { getOrder :: a }
  deriving (Bounded, Enum, Eq, Foldable, Functor, Lower, Num, Ord, Read, Show, Traversable, Upper)
