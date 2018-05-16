{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Semilattice.Tumble where

import Data.Semilattice.Join
import Data.Semilattice.Lower
import Data.Semilattice.Meet
import Data.Semilattice.Upper

newtype Tumble a = Tumble { getTumble :: a }
  deriving (Enum, Eq, Foldable, Functor, Num, Ord, Read, Show, Traversable)

instance Join a => Meet (Tumble a) where
  Tumble a /\ Tumble b = Tumble (a \/ b)

instance Meet a => Join (Tumble a) where
  Tumble a \/ Tumble b = Tumble (a /\ b)

instance Bounded a => Bounded (Tumble a) where
  minBound = Tumble maxBound
  maxBound = Tumble minBound

instance Lower a => Upper (Tumble a) where
  upperBound = Tumble lowerBound

instance Upper a => Lower (Tumble a) where
  lowerBound = Tumble upperBound
