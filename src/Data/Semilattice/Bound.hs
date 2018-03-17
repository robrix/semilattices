{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Semilattice.Bound where

import Data.Semilattice.Lower
import Data.Semilattice.Upper

newtype Bound a = Bound { getBound :: a }
  deriving (Bounded, Enum, Eq, Foldable, Functor, Num, Ord, Read, Show, Traversable)

instance Bounded a => Lower (Bound a)
instance Bounded a => Upper (Bound a)
