{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Util (
  withZero,
  ifThenElse, ifThenElse', divide, pass, copy'', isGE,
  printNibble', printHexByte', printNL, printDecimal
) where

import Instr   -- move, moveb, inc, ..., debug
import Address
import Allocator (compile, alloc, nalloc)
import Pair
import Translatable
import Control.Monad.Reader

clearPair pair = do clear pair; clear (second pair)

withZero zero body = runReaderT body zero

-- if x is not zero perform thenClause else perform elseClause
-- x+1 must be non-zero if x is zero
-- zero and zero+1 both must contain zero
ifThenElse' x thenClause elseClause = do
  zero <- ask
  at x open
  thenClause
  move zero
  close
  moverel 1
  open
  moveb (translate 1 x)
  elseClause
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
      (return ())
      (do dotimes' r' (incr r)
          incr q)

pass = return()

-- destructive copy
copy'' x y = do
  clear y
  dotimes' x (incr y)

-- set r to 1 if px >= c
isGE c px result = do
  alloc $ \t -> do
  alloc $ \s -> do
  assign result 1
  assign t c
  while t $ do
    ifThenElse px
      pass
      (do clear result; clear t)
    decr px
    decr t
    incr s
  dotimes' s (incr px)

printNibble' px = do
  alloc $ \r -> do
  isGE 10 px r
  incr_by px 48
  dotimes' r $ incr_by px 7
  putch px

printHexByte' px = do
  allocPair $ \pq -> do
  allocPair $ \pr -> do
  let pr' = second pr
  assign pr 16
  clear pr'
  clear pq
  divide px pr pq
  printNibble' pq
  copy'' pr' pq
  printNibble' pq

printNL = do
  alloc $ \t -> do
  assign t 10
  putch t

printDecimal a work = do
  -- a    = address of value to print
  -- work = address of work space to use

  -- frame offsets:
  let zero    = Pair 0  -- a pair of zeros
      isdigit = R 2     -- == 1 if frame contains a digit
      x       = R 3     -- current value
      r       = Pair 4  -- remainder pair
      r'      = second r
      frameSize = 6

      advance = moverel frameSize
      backup  = moverel (-frameSize)
      next v  = translate frameSize v

  -- copy the input value to field x of frame 1
  let x1 = next (translate (addr work) x)
  clear x1
  dotimes' a (incr x1)   -- destructive copy of a

  at work $ do
    -- now all addresses are relative to work
    clearPair zero
    withZero zero $ do
      clear isdigit      -- clear isdigit of frame 0
      advance
      while x $ do
        assign isdigit 1
        assign r 10
        clear r'
        divide x r (next x)
        advance
      backup
      while isdigit $ do
        incr_by r' 48
        putch r'
        backup
