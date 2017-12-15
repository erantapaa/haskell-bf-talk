{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Step3 (
  Instr(..), simplify, toOpcodes,
  BF, compile, R(..),
  move, moveb, inc, readch, printch, open, close, debug,
  at, clear, incr_by, dotimes'
) where

import BFOps  -- defines Instr(..), simplify, toOpcodes

import Control.Monad.Writer.Strict
import Data.DList hiding (replicate)

-- our monad
type BF = Writer (DList Instr)

-- compile a BF instruction sequence
compile :: BF a -> String
compile prog = toOpcodes (simplify (toList instrs))
  where (_, instrs) = runWriter prog

-- a value representing a memory location
data R = R Int

class Memory a where
  addr :: a -> Int

instance Memory R where
  addr (R x) = x

-- define monadic versions of the basic instructions
move x    = tell (singleton (Move (addr x)))
moveb x   = tell (singleton (Move (-(addr x))))

-- unchanged:
inc a     = tell (singleton (Inc a))
readch    = tell (singleton Read )
printch   = tell (singleton Print)
open      = tell (singleton Open)
close     = tell (singleton Close)
debug msg = tell (singleton (Debug msg))

at x body = do
  move x
  body
  moveb x   -- changed

-- same as before:
clear x     = at x $ do open; inc (-1); close
incr_by x a = at x (inc a)
dotimes' x body = do
  at x open
  body
  at x $ do inc (-1); close

-- print A
ex1 = do incr_by x 10
         dotimes' x (incr_by y 6)
         incr_by x 5
         at x printch
         at x (debug "here")
  where x = R 0
        y = R 1

