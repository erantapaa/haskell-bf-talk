{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Memory where

-- a value representing a memory location
data R = R Int

class Memory a where
  addr :: a -> Int

instance Memory R where
  addr (R x) = x

