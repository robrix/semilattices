{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Join where

import Data.Lower
import Data.Semigroup
import qualified Data.Set as Set

-- | A join semilattice is an idempotent commutative semigroup.
class Join s where
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
  --   Additionally, if @s@ has a 'Lower' bound, then 'bottom' must be its left- and right-identity.
  --
  --   > bottom \/ a = a
  --   > a \/ bottom = a
  (\/) :: s -> s -> s

  infixr 6 \/


instance Join () where
  _ \/ _ = ()

instance Join Bool where
  (\/) = (||)

instance Ord a => Join (Max a) where
  (\/) = (<>)

instance Ord a => Join (Set.Set a) where
  (\/) = Set.union


newtype Joining a = Joining { getJoining :: a }
  deriving (Enum, Eq, Foldable, Functor, Join, Lower, Num, Ord, Read, Show, Traversable)

instance Join a => Semigroup (Joining a) where
  (<>) = (\/)

instance (Lower a, Join a) => Monoid (Joining a) where
  mappend = (<>)
  mempty = bottom
