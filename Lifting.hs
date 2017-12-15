{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Lifting where

import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Reader

import Data.DList (DList)
import BFOps (Instr(..))
import Instr

-- This is an example of how mtl automatically lifts monadic expressions.
-- Recall: printch = tell (singleton PrintChar)

test1 = let _ = printch :: Writer (DList Instr) () in 1
test2 = let _ = printch :: StateT Int (Writer (DList Instr)) () in 2
test3 = let _ = printch :: ReaderT Char (StateT Int (Writer (DList Instr))) () in 3

