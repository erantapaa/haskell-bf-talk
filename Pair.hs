{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Pair where

import Address
import Allocator (nalloc)

data Pair = Pair Int
  deriving (Read, Show, Eq)

instance Address Pair where
  addr (Pair x) = x

second :: Pair -> R
second (Pair x) = R (x+1)

allocPair body = nalloc 2 $ \x -> body (Pair x)
