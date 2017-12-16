{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

-- switch statements, multi-byte numbers

module Step9 where

-- import Address
import Instr    -- move, moveb, inc, ..., debug
import Allocator (compile, alloc, nalloc)
import Pair
-- import Translatable
import Util

clearPair pair = do clear pair; clear (second pair)

-- increment a multi-byte number
incrPairs [] = return ()
incrPairs (x:xs) = do
  incr x
  ifThenElse x
    pass
    (incrPairs xs)

-- decrement a multi-byte number
decrPairs [] = return ()
decrPairs (x:xs) = do
  ifThenElse x
    pass
    (decrPairs xs)
  decr x

-- perform body if all pairs are zero
allZero [] body = body
allZero (x:xs) body = do
  ifThenElse x
    pass
    (allZero xs body)

-- demo of a two-byte number: count down from 0x123 to 1
test_twobytes = do
  allocPair $ \zero -> do
  allocPair $ \x0 -> do
  allocPair $ \x1 -> do
  alloc $ \notDone -> do
  withZero zero $ do
  assign x1 0x01      -- hi byte
  assign x0 0x23      -- low byte
  -- [x0, x1] represents 0x0123
  assign notDone 1
  while notDone $ do
    debug x1 "x1"
    debug x0 "x0"
    decrPairs [x0, x1]
    allZero [x0, x1] (clear notDone)
 
