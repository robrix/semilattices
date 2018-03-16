{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Tumble where

import Control.Applicative
import Control.Monad.Fix
import Data.Coerce
import Data.Join
import Data.Lower
import Data.Meet
import Data.Upper

newtype Tumble a = Tumble { getTumble :: a }
  deriving (Enum, Eq, Foldable, Functor, Num, Ord, Read, Show, Traversable)

instance Applicative Tumble where
  pure = Tumble
  a <* _ = a
  _ *> a = a
  (<*>) = coerce
  liftA2 = coerce

instance Monad Tumble where
  (>>) = (*>)
  Tumble a >>= f = f a

instance MonadFix Tumble where
  mfix f = fix (f . getTumble)

instance Join a => Meet (Tumble a) where
  Tumble a /\ Tumble b = Tumble (a \/ b)

instance Meet a => Join (Tumble a) where
  Tumble a \/ Tumble b = Tumble (a /\ b)

instance Bounded a => Bounded (Tumble a) where
  minBound = Tumble maxBound
  maxBound = Tumble minBound

instance Lower a => Upper (Tumble a) where
  top = Tumble bottom

instance Upper a => Lower (Tumble a) where
  bottom = Tumble top
