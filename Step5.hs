{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Step5 where

import BFOps  -- defines Instr(..), simplify, toOpcodes

import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Data.DList hiding (replicate)

import Address
import Instr   -- move, moveb, inc, ..., debug

data AllocState = AllocState { freeptr :: !Int, max_freeptr :: !Int }

initialAllocState = AllocState 0 0 -- initial allocator state

-- our monad
type BF = StateT AllocState (Writer (DList Instr))

compile :: BF a -> String
compile program =
  toOpcodes $ simplify $ toList $ execWriter (execStateT program initialAllocState)

-- allocate n memory cells and pass location to body
nalloc :: Int -> (Int -> BF a) -> BF a
nalloc n body = do
  st <- get
  let x = freeptr st
      end = x+n
  let st' = st { freeptr = end, max_freeptr = max (max_freeptr st) end }
  put st'
  a <- body x
  let st'' = st' { freeptr = x }
  put st''
  return a

-- allocate a single memory cell for use in body
alloc body = do
  nalloc 1 $ \x -> body (R x)

-- non-destructive dotimes
dotimes x body = do
  alloc $ \t -> do
    clear t
    dotimes' x (do body; incr t)
    dotimes' t (incr x)

test_dotimes = do
  alloc $ \ch_pound -> do   -- ch_pound = R 0
  alloc $ \ch_nl -> do      -- ch_nl    = R 1
  alloc $ \x -> do          -- x        = R 2
  assign ch_nl 10
  assign ch_pound 35        -- ascii value for #
  assign x 10
  debug x "x before"
  dotimes x (putch ch_pound)
  putch ch_nl
  debug x "x after"

-- alternate version of compile which also returns the
-- maximum memory allocated
compile' :: BF a -> (Int, String)
compile' program = 
  let ( (_, st), dlist ) = runWriter (runStateT program initialAllocState)
      code = toOpcodes $ simplify $ toList dlist
  in (max_freeptr st, code)

