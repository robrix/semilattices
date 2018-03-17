{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Tumble where

import Data.Join
import Data.Lower
import Data.Meet
import Data.Upper

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
  upper = Tumble lower

instance Upper a => Lower (Tumble a) where
  lower = Tumble upper
