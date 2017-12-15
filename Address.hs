{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Address where

-- a value representing a memory address
data R = R Int

class Address a where
  addr :: a -> Int

instance Address R where
  addr (R x) = x

