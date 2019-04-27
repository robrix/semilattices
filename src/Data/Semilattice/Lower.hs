{-# LANGUAGE CPP, DefaultSignatures, PolyKinds, TypeFamilies, TypeOperators #-}
-- | Lower bounds, related to 'Bounded', 'Join', 'Meet', and 'Ord'.
module Data.Semilattice.Lower where

import Data.Char
import Data.Coerce
import Data.Functor.Const
import Data.Functor.Identity
import Data.Int
import Data.HashMap.Lazy as HashMap
import Data.HashSet as HashSet
import Data.IntMap as IntMap
import Data.IntSet as IntSet
import Data.Map as Map
import Data.Monoid as Monoid
import Data.Proxy
import Data.Semigroup as Semigroup
import Data.Sequence as Seq
import Data.Set as Set
import Data.Type.Coercion
import Data.Type.Equality
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import GHC.Generics
import System.Posix.Types

-- | The greatest lower bound of @s@.
--
--   Laws:
--
--   If @s@ is 'Bounded', we require 'lowerBound' and 'minBound' to agree:
--
-- @
-- 'lowerBound' = 'minBound'
-- @
--
--   If @s@ is a 'Join' semilattice, 'lowerBound' must be the identity of '\/':
--
-- @
-- 'lowerBound' '\/' a = a
-- @
--
--   If @s@ is a 'Meet' semilattice, 'lowerBound' must be the absorbing element of '/\':
--
-- @
-- 'lowerBound' '/\' a = 'lowerBound'
-- @
--
--   If @s@ is 'Ord'ered, 'lowerBound' must be at least as small as every terminating value:
--
-- @
-- 'compare' 'lowerBound' a /= 'GT'
-- @
class Lower s where
  lowerBound :: s
  default lowerBound :: Bounded s => s
  lowerBound = minBound


-- Prelude
instance Lower ()

-- $
--
-- Bounded:
--
-- prop> lowerBound == (minBound :: Bool)
--
-- Identity of '\/':
--
-- prop> lowerBound \/ a == (a :: Bool)
--
-- Absorbing element of '/\':
--
-- prop> lowerBound /\ a == (lowerBound :: Bool)
--
-- Ord:
--
-- prop> compare lowerBound (a :: Bool) /= GT
instance Lower Bool
instance Lower Ordering
instance Lower Char
instance Lower Int
instance (Lower a, Lower b) => Lower (a, b) where lowerBound = (lowerBound, lowerBound)
instance (Lower a, Lower b, Lower c) => Lower (a, b, c) where lowerBound = (lowerBound, lowerBound, lowerBound)
instance (Lower a, Lower b, Lower c, Lower d) => Lower (a, b, c, d) where lowerBound = (lowerBound, lowerBound, lowerBound, lowerBound)
instance (Lower a, Lower b, Lower c, Lower d, Lower e) => Lower (a, b, c, d, e) where lowerBound = (lowerBound, lowerBound, lowerBound, lowerBound, lowerBound)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f) => Lower (a, b, c, d, e, f) where lowerBound = (lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g) => Lower (a, b, c, d, e, f, g) where lowerBound = (lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h) => Lower (a, b, c, d, e, f, g, h) where lowerBound = (lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i) => Lower (a, b, c, d, e, f, g, h, i) where lowerBound = (lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i, Lower j) => Lower (a, b, c, d, e, f, g, h, i, j) where lowerBound = (lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i, Lower j, Lower k) => Lower (a, b, c, d, e, f, g, h, i, j, k) where lowerBound = (lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i, Lower j, Lower k, Lower l) => Lower (a, b, c, d, e, f, g, h, i, j, k, l) where lowerBound = (lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i, Lower j, Lower k, Lower l, Lower m) => Lower (a, b, c, d, e, f, g, h, i, j, k, l, m) where lowerBound = (lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i, Lower j, Lower k, Lower l, Lower m, Lower n) => Lower (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where lowerBound = (lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i, Lower j, Lower k, Lower l, Lower m, Lower n, Lower o) => Lower (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where lowerBound = (lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound, lowerBound)
instance Lower b => Lower (a -> b) where lowerBound = const lowerBound

instance Lower (Maybe a) where lowerBound = Nothing
instance Lower [a] where lowerBound = []


-- Data.Char
instance Lower GeneralCategory

-- Data.Int
instance Lower Int8
instance Lower Int16
instance Lower Int32
instance Lower Int64

-- Data.Functor.Const
instance Lower a => Lower (Const a b) where lowerBound = Const lowerBound

-- Data.Functor.Identity
instance Lower a => Lower (Identity a) where lowerBound = Identity lowerBound

-- Data.Monoid
instance Lower All
instance Lower Any
instance Lower a => Lower (Product a) where lowerBound = Product lowerBound
instance Lower a => Lower (Sum a) where lowerBound = Sum lowerBound
instance Lower a => Lower (Dual a) where lowerBound = Dual lowerBound
instance Lower (Endo a) where lowerBound = Endo id
instance Lower (Monoid.First a) where lowerBound = mempty
instance Lower (Monoid.Last a) where lowerBound = mempty

-- Data.Proxy
instance Lower (Proxy a)

-- Data.Semigroup
instance Lower a => Lower (Semigroup.First a) where lowerBound = Semigroup.First lowerBound
instance Lower a => Lower (Semigroup.Last a) where lowerBound = Semigroup.Last lowerBound
instance Lower a => Lower (Max a) where lowerBound = Max lowerBound
instance Lower a => Lower (Min a) where lowerBound = Min lowerBound
instance Lower a => Lower (WrappedMonoid a) where lowerBound = WrapMonoid lowerBound

-- Data.Type.Coercion
instance Coercible a b => Lower (Coercion a b)

-- Data.Type.Equality
instance (a ~ b) => Lower (a :~: b)
#if MIN_VERSION_base(4,10,0)
instance (a ~~ b) => Lower (a :~~: b)
#endif

-- Data.Word
instance Lower Word8
instance Lower Word16
instance Lower Word32
instance Lower Word64

-- Foreign.C.Types
instance Lower CUIntMax
instance Lower CIntMax
instance Lower CUIntPtr
instance Lower CIntPtr
instance Lower CSigAtomic
instance Lower CWchar
instance Lower CSize
instance Lower CPtrdiff

instance Lower CULLong
instance Lower CLLong
instance Lower CULong
instance Lower CLong
instance Lower CUInt
instance Lower CInt
instance Lower CUShort
instance Lower CShort
instance Lower CUChar
instance Lower CSChar
instance Lower CChar

#if MIN_VERSION_base(4,10,0)
instance Lower CBool
#endif

-- Foreign.Ptr
instance Lower IntPtr
instance Lower WordPtr

-- GHC.Generics
instance Lower DecidedStrictness
instance Lower SourceStrictness
instance Lower SourceUnpackedness
instance Lower Associativity

-- System.Posix.Types
instance Lower Fd
instance Lower CRLim
instance Lower CTcflag
instance Lower CUid
instance Lower CNlink
instance Lower CGid
instance Lower CSsize
instance Lower CPid
instance Lower COff
instance Lower CMode
instance Lower CIno
instance Lower CDev

#if MIN_VERSION_base(4,10,0)
instance Lower CKey
instance Lower CId
instance Lower CFsFilCnt
instance Lower CFsBlkCnt
#ifdef HTYPE_CLOCKID_T
instance Lower CClockId
#endif
instance Lower CBlkCnt
instance Lower CBlkSize
#endif

-- containers
instance Lower (IntMap a) where lowerBound = IntMap.empty
instance Lower IntSet where lowerBound = IntSet.empty
instance Lower (Map k a) where lowerBound = Map.empty
instance Lower (Seq a) where lowerBound = Seq.empty
instance Lower (Set a) where lowerBound = Set.empty

-- unordered-containers
instance Lower (HashMap k a) where lowerBound = HashMap.empty
instance Lower (HashSet a) where lowerBound = HashSet.empty


-- $setup
-- >>> import Data.Semilattice.Join
-- >>> import Data.Semilattice.Meet
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> import Test.QuickCheck.Function
