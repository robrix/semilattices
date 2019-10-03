{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
-- | 'Lower' and 'Upper' bounds from 'Bounded' instances.
module Data.Semilattice.Bound
( Bound(..)
) where

import Data.Semilattice.Lower
import Data.Semilattice.Upper

-- | A convenience bridging 'Bounded' to 'Lower' and 'Upper'.
newtype Bound a = Bound { getBound :: a }
  deriving (Bounded, Enum, Eq, Foldable, Functor, Num, Ord, Read, Show, Traversable)

instance Bounded a => Lower (Bound a)
instance Bounded a => Upper (Bound a)
