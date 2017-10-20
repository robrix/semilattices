module Data.Semilattice where

import Data.Semigroup

class (Monoid s, Semigroup s) => Semilattice s
