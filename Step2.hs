{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Step2 where

import BFOps  -- defines Instr(..), simplify, toOpcodes

import Control.Monad.Writer.Strict
import Data.DList hiding (replicate)

-- our monad
type BF = Writer (DList Instr)

-- compile a BF instruction sequence
compile :: BF a -> String
compile prog = toOpcodes ( simplify (toList instrs))
  where (_, instrs) = runWriter prog

-- define monadic versions of the basic instructions
move x    = tell (singleton (Move x))
inc a     = tell (singleton (Inc a))
readch    = tell (singleton Read )
printch   = tell (singleton Print)
open      = tell (singleton Open)
close     = tell (singleton Close)
debug msg = tell (singleton (Debug msg))

-- "Return to Zero" protocol
at x body = do
  move x
  body
  move (-x)

clear x     = at x $ do open; inc (-1); close
incr_by x a = at x (inc a)
dotimes' x body = do
  at x open
  body
  at x $ do inc (-1); close

-- print A
ex1 = do incr_by 0 10
         dotimes' 0 (incr_by 1 6)
         incr_by 1 5
         at 1 printch
         at 1 (debug "here")

-- with named memory locations
ex2 = do incr_by x 10
         dotimes' x (incr_by y 6)
         incr_by x 5
         at x printch
         at x (debug "here")
  where x = 0
        y = 1

