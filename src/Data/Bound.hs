{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Bound where

import Data.Lower
import Data.Upper

newtype Bound a = Bound { getBound :: a }
  deriving (Bounded, Enum, Eq, Foldable, Functor, Num, Ord, Read, Show, Traversable)

instance Bounded a => Lower (Bound a) where
  bottom = minBound

instance Bounded a => Upper (Bound a) where
  top = maxBound
