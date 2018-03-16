{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Semilattice where

import Control.Applicative
import Control.Monad.Fix
import Data.Coerce
import Data.Data
import Data.Join
import Data.Meet
import qualified Data.Semigroup as Semigroup
import qualified Data.Set as Set
import GHC.Generics

class Lower s where
  -- | The greatest lower bound of @s@.
  --
  --   Laws:
  --
  --   If @s@ is 'Bounded', we require 'bottom' and 'minBound' to agree:
  --
  --   > bottom = minBound
  --
  --   If @s@ is a 'Join', 'bottom' must be the identity of '(\/)':
  --
  --   > bottom \/ a = a
  --
  --   If @s@ is 'Ord'ered, 'bottom' must be at least as small as every terminating value:
  --
  --   > compare bottom a /= GT
  bottom :: s

class UpperBound s where
  -- | The least upper bound of @s@.
  --
  --   Laws:
  --
  --   If @s@ is 'Bounded', we require 'top' and 'maxBound' to agree:
  --
  --   > top = maxBound
  --
  --   If @s@ is a 'Meet', 'top' must be the identity of '(/\)':
  --
  --   > top \/ a = a
  --
  --   If @s@ is 'Ord'ered, 'top' must be at least as large as every terminating value:
  --
  --   > compare top a /= LT
  top :: s


instance Lower () where
  bottom = ()

instance UpperBound () where
  top = ()


instance Lower Bool where
  bottom = False

instance UpperBound Bool where
  top = True


instance Bounded a => Lower (Semigroup.Max a) where
  bottom = minBound


instance Bounded a => UpperBound (Semigroup.Min a) where
  top = maxBound


instance Lower (Set.Set a) where
  bottom = Set.empty


newtype Tumble a = Tumble { getTumble :: a }
  deriving (Bounded, Data, Enum, Eq, Foldable, Functor, Generic, Generic1, Num, Ord, Read, Show, Traversable)

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

instance Lower a => UpperBound (Tumble a) where
  top = Tumble bottom

instance UpperBound a => Lower (Tumble a) where
  bottom = Tumble top
