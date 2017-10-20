{-# LANGUAGE DefaultSignatures #-}
module Data.Semilattice where

class Semilattice s where
  bottom :: s
  default bottom :: Monoid s => s
  bottom = mempty

  (\/) :: s -> s -> s
  default (\/) :: Monoid s => s -> s -> s
  (\/) = mappend
