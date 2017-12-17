{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

-- Arrays

module Array (
  withFrameContext,
  advance, backup, next, prev,
  advanceToTarget, rewindTo0,
  setupBreadCrumbs,
  eraseCrumbsFrom0, eraseCrumbsFromTarget,
  toFrameContext,
  arrayIndex, arrayValue,
  arrayCrumbOffset, arrayIndexOffset, arrayValueOffset,
  arrayPut, arrayGet, arrayIncr, arrayDecr
) where

import Instr    -- move, moveb, inc, ..., debug
import Address
import Translatable
import Allocator (compile, alloc, nalloc)
import Translatable
import Control.Monad.Reader
import Pair
import Util (copy, printDecimal, withZero)

data FrameContext = FrameContext { frameSize_ :: Int }
  deriving (Read, Show)

advance = do
  fctx <- ask
  moverel (frameSize_ fctx)

backup = do
  fctx <- ask
  moverel (negate $ frameSize_ fctx)

next x = do
  fctx <- ask
  translate (frameSize_ fctx) x

prev x = do
  fctx <- ask
  translate (negate $ frameSize_ fctx) x

withFrameContext fctx body = runReaderT body fctx

setupBreadCrumbs i crumb = do
  -- i is the address of the index in frame 0
  -- crumb is the address of crumb cell in all frames
  -- destroys i
  dotimes' i $ do
    advance
    while crumb advance
    incr crumb
    while crumb backup

data BFArray = BFArray { arrayStart_ :: R, arrayWidth_ :: Int }

instance Address BFArray where
  addr arr = addr (arrayStart_ arr)

instance Translatable BFArray where
  translate n arr = arr { arrayStart_ = translate n (arrayStart_ arr) }

toFrameContext arr = FrameContext (arrayWidth_ arr)

arrayIndex arr = translate (addr arrayIndexOffset) arr
arrayValue arr = translate (addr arrayValueOffset) arr

arrayCrumbOffset = R 0
arrayIndexOffset = R 1 -- offset of index value in frame 0
arrayValueOffset = R 2 -- offset of the value cell in frame 0

advanceToTarget = do
  advance
  while arrayCrumbOffset advance

rewindTo0 = do
  backup
  while arrayCrumbOffset backup

eraseCrumbsFrom0 = do
  advanceToTarget
  eraseCrumbsFromTarget

-- erase crumbs when the data pointer is already
-- at the target cell
eraseCrumbsFromTarget = do
  backup
  while arrayCrumbOffset $ do clear arrayCrumbOffset; backup

arrayPut arr dest = do
  -- On entry:
  --   arrayIndex arr  contains the index to perform the store on
  --   arrayValue arr  contains the value to store
  --   dest            is the offset within the array element to
  --                   store the value
  at arr $ do
    withFrameContext (toFrameContext arr) $ do
      setupBreadCrumbs arrayIndexOffset arrayCrumbOffset
      -- clear the target cell
      advanceToTarget
      clear dest
      rewindTo0
      dotimes' arrayValueOffset $ do
        advanceToTarget
        -- at the cell to modify
        incr dest
        rewindTo0
      -- clean up the crumbs
      eraseCrumbsFrom0

arrayGet arr dest tmp = do
  -- On entry:
  --   arrayIndex arr  contains the index to perform the store on
  --   dest            is the offset within the array element of
  --                   the value to get
  -- On exit:
  --   arrayValue arr  contains the fetched value
  at arr $ do
    withFrameContext (toFrameContext arr) $ do
      setupBreadCrumbs arrayIndexOffset arrayCrumbOffset
      clear arrayValueOffset
      advanceToTarget
      clear tmp
      dotimes' dest $ do
        incr tmp
        rewindTo0
        incr arrayValueOffset
        advanceToTarget
      -- clear the crumbs
      dotimes' tmp $ incr dest
      eraseCrumbsFromTarget

-- increment the target cell by `arrayValue arr`
arrayIncr arr dest = do
  at arr $ do
    withFrameContext (toFrameContext arr) $ do
      setupBreadCrumbs arrayIndexOffset arrayCrumbOffset
      dotimes' arrayValueOffset $ do
        advanceToTarget
        incr dest
        rewindTo0
      -- clean up the crumbs
      eraseCrumbsFrom0

-- decrement the target cell by `arrayValue arr`
arrayDecr arr dest = do
  at arr $ do
    withFrameContext (toFrameContext arr) $ do
      setupBreadCrumbs arrayIndexOffset arrayCrumbOffset
      dotimes' arrayValueOffset $ do
        advanceToTarget
        decr dest
        rewindTo0
      -- clean up the crumbs
      eraseCrumbsFrom0

-- read from stdin and report the number of
-- times each character appears
example1 = do
  allocPair $ \zero -> do
  withZero zero $ do
  alloc $ \ch -> do
  alloc $ \ch_space -> do
  alloc $ \ch_nl -> do
  allocPair $ \x -> do

  -- allocate work space for printDecimal
  nalloc 26 $ \w -> do
  let work = R w

  assign ch_space 32
  assign ch_nl 10

  -- debug work "address of work"

  let arr = BFArray (R 50) 3 -- start array at address 50
      countOffset = R 1
      tmpOffset   = R 2
  getch ch
  while ch $ do
    copy ch (arrayIndex arr)
    assign (arrayValue arr) 1
    arrayIncr arr countOffset
    getch ch

  assign ch 1
  while ch $ do
    copy ch (arrayIndex arr)
    arrayGet arr countOffset tmpOffset

    while (arrayValue arr) $ do
      putch ch
      putch ch_space
      printDecimal (arrayValue arr) work
      putch ch_nl
      clear (arrayValue arr)

    incr ch

