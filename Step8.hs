{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Step8 where

import Address
import Instr    -- move, moveb, inc, ..., debug
import Allocator (compile, alloc, nalloc)
import Pair
import Translatable
import Step7    -- ifThenElse, withZero, divide

clearPair pair = do clear pair; clear (second pair)

-- print a number in decimal
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

  -- memory used at work: frameSize + frameSize*ndigits

test_printDecimal v = do
  alloc $ \a -> do
  let work = R 10
  assign a v
  printDecimal a work
  assign a 10
  putch a

