{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Step7 (
  ifThenElse', ifThenElse, divide, withZero
) where

import Instr   -- move, moveb, inc, ..., debug
import Allocator (compile, alloc, nalloc)
import Pair
import Translatable

import Control.Monad.Reader

withZero zero body = runReaderT body zero

-- if x is not zero perform thenClause else perform elseClause
-- x+1 must be non-zero if x is zero
-- zero and zero+1 both must contain zero
ifThenElse' x thenClause elseClause = do
  zero <- ask
  at x open
  elseClause
  move zero
  close
  moverel 1
  open
  moveb (translate 1 x)
  thenClause
  at (translate 1 zero) close

ifThenElse x thenClause elseClause = do
  assign (translate 1 x) 1
  ifThenElse' x thenClause elseClause

-- divide x by r
-- quotient is returned in q
-- remainder is returned in r'
divide x r q = do
  let r' = translate 1 r
      x' = translate 1 x
  clear q
  clear r'
  -- invariant: r + r' is constant (the divisor)
  -- so if r == 0 then r' /= 0
  dotimes' x $ do
    decr r
    incr r'
    ifThenElse' r
      (do dotimes' r' (incr r)
          incr q)
      (return ())

test_divide a b = do
  allocPair $ \zero -> do
  withZero zero $ do
    allocPair $ \x    -> do
    allocPair $ \r    -> do
    alloc     $ \q    -> do
    assign x a        -- note: x and r are Pairs!
    assign r b
    divide x r q
    debug q "quotient"
    debug (translate 1 r) "remainder"

