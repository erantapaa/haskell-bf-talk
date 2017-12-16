{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Instr (
  moverel, move, moveb, inc, readch, printch, open, close, debug, debug_,
  at, clear, incr_by, decr_by, dotimes', assign, while, 
  incr, decr, getch, putch
) where

import BFOps  -- defines Instr(..), simplify, toOpcodes
import Control.Monad.Writer.Strict
import Data.DList hiding (replicate)
import Address

-- define monadic versions of the basic instructions
moverel n = tell (singleton (Move n))
move x    = moverel (addr x)
moveb x   = moverel (-(addr x))

-- unchanged:
inc a     = tell (singleton (Inc a))
readch    = tell (singleton Read )
printch   = tell (singleton Print)
open      = tell (singleton Open)
close     = tell (singleton Close)
debug_ msg = tell (singleton (Debug msg))

at x body = do
  move x
  body
  moveb x   -- changed

debug x msg = at x (debug_ msg)

-- same as before:
clear x     = at x $ do open; inc (-1); close

incr_by x a = at x (inc a)
decr_by x a = at x (inc (-a))

dotimes' x body = do
  at x open
  body
  at x $ do inc (-1); close

assign x a = do
  clear x
  incr_by x a

-- perform body while x
while x body = do
  at x open
  body
  at x close

incr x = at x (inc 1)
decr x = at x (inc (-1))
getch x = at x readch
putch x = at x printch

{-
-- non-destructive dotimes
dotimes x body t = do
  clear t
  dotimes' x (do body; incr t)
  dotimes' t (incr x)

-- non-destructive copy
copy x y t = do
  clear y
  dotimes x (incr y) t
-}

