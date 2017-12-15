{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Step6 where

import BFOps  -- defines Instr(..), simplify, toOpcodes

import Memory  -- R
import Instr   -- move, moveb, inc, ..., debug
import Allocator (compile, alloc, nalloc)

class Translatable a where
  translate :: Int -> a -> a

-- memory addresses are translatable
instance Translatable R where
  translate n (R x) = R (x+n)

-- if x is not zero perform thenClause else perform elseClause
-- x+1 must be non-zero if x is zero
-- zero and zero+1 both must contain zero
ifThenElse' x zero thenClause elseClause = do
  at x open
  elseClause
  move zero
  close
  moverel 1
  open
  moveb (translate 1 x)
  thenClause
  at (translate 1 zero) close

{-
    Data Pointer Location:

                    x /= 0      x == 0
    move x            x           x
    open              x           .
      moveb x         0           .
      elseClause      0           .
      move z          z           .
    close             z           .   
    moverel 1        z+1         x+1
    open             z+1         x+1 **
      moveb (x+1)     .           0
      thenClause      .           0
      move  (z+1)     .          z+1        
    close             .          z+1
    moveb (z+1)       0           0

    +-----+-----+--- ---+-----+-----+
    |  0  |  1  |  ...  |  0  |  0  |
    +-----+-----+--- ---+-----+-----+
       x    x+1            z    z+1

-}

-- divide x by r
-- quotient is returned in q
-- remainder is returned in r'
divide x r q zero = do
  let r' = translate 1 r
      x' = translate 1 x
  clear q
  clear r'
  -- invariant: r + r' is constant (the divisor)
  -- so if r == 0 then r' /= 0
  dotimes' x $ do
    decr r
    incr r'
    ifThenElse' r zero
      (do dotimes' r' (incr r)
          incr q)
      (return ())


-- divide a by b
test_divide a b = do
  alloc $ \zero -> do  -- zero = R 0
  alloc $ \_ -> do
  alloc $ \x -> do     -- x    = R 2
  alloc $ \_ -> do
  alloc $ \r -> do     -- r    = R 4
  alloc $ \_ -> do
  alloc $ \q -> do     -- q    = R 6
  assign x a
  assign r b
  divide x r q zero
  debug q "quotient"
  debug (translate 1 r) "remainder"


-- always assures x+1 is non-zero
ifThenElse x zero thenClause elseClause = do
  assign (translate 1 x) 1
  ifThenElse' x zero thenClause elseClause

-- Pair represents two adjacent memory addresses
data Pair = Pair Int
  deriving (Read, Show, Eq)

instance Memory Pair where
  addr (Pair x) = x

instance Translatable Pair where
  translate n (Pair x) = Pair (x+n)

-- second access the second cell of a pair
second :: Pair -> R
second (Pair x) = R (x+1)

-- allocate a pair
allocPair body = nalloc 2 $ \x -> body (Pair x)

-- by making Pair an instance of Memory we use them
-- with the basic BF instructions:

test_divide' a b = do
  allocPair $ \zero -> do
  allocPair $ \x    -> do
  allocPair $ \r    -> do
  alloc     $ \q    -> do
  assign x a        -- note: x and r are Pairs!
  assign r b
  divide x r q zero
  debug q "quotient"
  debug (translate 1 r) "remainder"

