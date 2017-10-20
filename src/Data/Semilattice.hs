{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Semilattice where

import Control.Monad.Fix
import Data.Data
import qualified Data.Set as Set
import GHC.Generics

-- | A join semilattice is an idempotent commutative semigroup.
class JoinSemilattice s where
  -- | The join operation.
  --
  --   Laws:
  --
  --   Idempotence:
  --
  --   > x \/ x = x
  --
  --   Associativity:
  --
  --   > a \/ (b \/ c) = (a \/ b) \/ c
  --
  --   Commutativity:
  --
  --   > a \/ b = b \/ a
  --
  --   Additionally, if @s@ has a 'LowerBound', the identity law must hold:
  --
  --   > bottom \/ a = a
  (\/) :: s -> s -> s

class MeetSemilattice s where
  -- | The meet operation.
  --
  --   Laws:
  --
  --   Idempotence:
  --
  --   > x /\ x = x
  --
  --   Associativity:
  --
  --   > a /\ (b /\ c) = (a /\ b) /\ c
  --
  --   Commutativity:
  --
  --   > a /\ b = b /\ a
  --
  --   Additionally, if @s@ has an 'UpperBound', the identity law must hold:
  --
  --   > top /\ a = a
  (/\) :: s -> s -> s

class LowerBound s where
  -- | The greatest lower bound of @s@.
  bottom :: s

class UpperBound s where
  -- | The least upper bound of @s@.
  top :: s


instance JoinSemilattice () where
  _ \/ _ = ()

instance MeetSemilattice () where
  _ /\ _ = ()

instance LowerBound () where
  bottom = ()

instance UpperBound () where
  top = ()


instance Ord a => JoinSemilattice (Set.Set a) where
  (\/) = Set.union

instance Ord a => MeetSemilattice (Set.Set a) where
  (/\) = Set.intersection

instance LowerBound (Set.Set a) where
  bottom = Set.empty


newtype Tumble a = Tumble { getTumble :: a }
  deriving (Bounded, Data, Enum, Eq, Foldable, Functor, Generic, Generic1, Num, Ord, Read, Show, Traversable)

instance Applicative Tumble where
  pure = Tumble
  Tumble f <*> Tumble a = Tumble (f a)

instance Monad Tumble where
  Tumble a >>= f = f a

instance MonadFix Tumble where
  mfix f = fix (f . getTumble)

instance JoinSemilattice a => MeetSemilattice (Tumble a) where
  Tumble a /\ Tumble b = Tumble (a \/ b)

instance MeetSemilattice a => JoinSemilattice (Tumble a) where
  Tumble a \/ Tumble b = Tumble (a /\ b)

instance LowerBound a => UpperBound (Tumble a) where
  top = Tumble bottom

instance UpperBound a => LowerBound (Tumble a) where
  bottom = Tumble top
