{-# LANGUAGE DefaultSignatures, PolyKinds, TypeFamilies, TypeOperators #-}
module Data.Lower where

import Data.Char
import Data.Coerce
import Data.Functor.Const
import Data.Functor.Identity
import Data.Int
import Data.IntMap as IntMap
import Data.Map as Map
import Data.Monoid as Monoid
import Data.Proxy
import Data.Semigroup as Semigroup
import Data.Set as Set
import Data.Type.Coercion
import Data.Type.Equality
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import GHC.Generics
import System.Posix.Types

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
  default bottom :: Bounded s => s
  bottom = minBound


-- Prelude
instance Lower ()
instance Lower Bool
instance Lower Ordering
instance Lower Char
instance Lower Int
instance (Lower a, Lower b) => Lower (a, b) where bottom = (bottom, bottom)
instance (Lower a, Lower b, Lower c) => Lower (a, b, c) where bottom = (bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d) => Lower (a, b, c, d) where bottom = (bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e) => Lower (a, b, c, d, e) where bottom = (bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f) => Lower (a, b, c, d, e, f) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g) => Lower (a, b, c, d, e, f, g) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h) => Lower (a, b, c, d, e, f, g, h) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i) => Lower (a, b, c, d, e, f, g, h, i) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i, Lower j) => Lower (a, b, c, d, e, f, g, h, i, j) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i, Lower j, Lower k) => Lower (a, b, c, d, e, f, g, h, i, j, k) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i, Lower j, Lower k, Lower l) => Lower (a, b, c, d, e, f, g, h, i, j, k, l) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i, Lower j, Lower k, Lower l, Lower m) => Lower (a, b, c, d, e, f, g, h, i, j, k, l, m) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i, Lower j, Lower k, Lower l, Lower m, Lower n) => Lower (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i, Lower j, Lower k, Lower l, Lower m, Lower n, Lower o) => Lower (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom)

instance Lower (Maybe a) where bottom = Nothing
instance Lower [a] where bottom = []


-- Data.Char
instance Lower GeneralCategory

-- Data.Int
instance Lower Int8
instance Lower Int16
instance Lower Int32
instance Lower Int64

-- Data.Functor.Const
instance Lower a => Lower (Const a b) where bottom = Const bottom

-- Data.Functor.Identity
instance Lower a => Lower (Identity a) where bottom = Identity bottom

-- Data.Monoid
instance Lower All
instance Lower Any
instance Lower a => Lower (Product a) where bottom = Product bottom
instance Lower a => Lower (Sum a) where bottom = Sum bottom
instance Lower a => Lower (Dual a) where bottom = Dual bottom

-- Data.Proxy
instance Lower (Proxy a)

-- Data.Semigroup
instance Lower a => Lower (Semigroup.First a) where bottom = Semigroup.First bottom
instance Lower a => Lower (Semigroup.Last a) where bottom = Semigroup.Last bottom
instance Lower a => Lower (Max a) where bottom = Max bottom
instance Lower a => Lower (Min a) where bottom = Min bottom
instance Lower a => Lower (WrappedMonoid a) where bottom = WrapMonoid bottom

-- Data.Type.Coercion
instance Coercible a b => Lower (Coercion a b)

-- Data.Type.Equality
instance a ~ b => Lower (a :~: b)
instance a ~~ b => Lower (a :~~: b)

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
instance Lower CBool
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
instance Lower CKey
instance Lower CId
instance Lower CFsFilCnt
instance Lower CFsBlkCnt
instance Lower CClockId
instance Lower CBlkCnt
instance Lower CBlkSize
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

-- containers
instance Lower (IntMap a) where bottom = IntMap.empty
instance Lower (Map k a) where bottom = Map.empty
instance Lower (Set a) where bottom = Set.empty
