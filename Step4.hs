{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

-- Some BF coding techniques

module Step4 where

import Step3

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

-- non-destructive dotimes
dotimes x body t = do
  clear t
  dotimes' x (do body; incr t)
  dotimes' t (incr x)

-- non-destructive copy
copy x y t = do
  clear y
  dotimes x (incr y) t

-- perform body if x is not zero
ifNonZero' x body = do
  while x (do body; clear x)

-- perform body if x is zero
ifZero' x body t = do
  assign t 1
  ifNonZero' x (clear t)
  dotimes' t body

-- destructive if-then-else using temporaries
ifThenElse' x thenClause elseClause t1 t2 = do
  clear t1
  assign t2 1
  ifNonZero' x (do incr t1; clear t2)
  ifNonZero' t1 thenClause
  ifNonZero' t2 elseClause

-- main problem with this if-then-else is that
-- it is destructive and inefficient.

