{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

-- switch statements

module Step10 where

import Instr    -- move, moveb, inc, ..., debug
import Allocator (compile, alloc, nalloc)
import Pair
import Util
import Control.Monad

-- a switch statement; destroys x
switch' x vars (case0 : cases ) =
  let adjust1 (v,c,t)  = incr_by v (t-c)
      adjust curr target = forM_ (zip3 vars curr target) adjust1 
      go curr [ ]     = clear x
      go curr (t:ts)  = do
        while x $ do
          decr x; adjust curr t; go t ts

  in do forM_ (zip vars case0) $ \(v,t) -> assign v t
        go case0 cases

switch_ex1 a = do
  allocPair $ \zero -> do
  withZero zero $ do
  allocPair $ \x -> do
  allocPair $ \y -> do
  allocPair $ \z -> do
  assign x a
  switch' x [y,z]
     [ [0, 0]   -- x == 0
     , [2, 1]   -- x == 1
     , [4, 1]   -- x == 2
     , [6, 2]   -- x == 3
     , [8, 3]   -- x == 4
     , [10, 5]  -- x == 5
     , [12, 8]  -- x == 6
     , [14, 13] -- x >= 7
     ]
  debug y "y"   -- should be 2*x
  debug z "z"   -- should be fib x

{-
   Code generated for the above switch statement:

   assign y 0         --  set up y,z for case x == 0
   assign z 0
   while x $ do
     decr x
     incr_by y 2      -- adjust y,z for case x == 1
     incr    z
     while x $ do
       decr x
       incr_by y 2    -- adjust for case x == 2
       while x $ do
         decr x
         incr_by y 2  -- adjust for case x == 3
         incr    z
         while x $ do
           ...
           clear x
-}
-- Note: Could also use ifThenElse for a non-destructive switch statement.

