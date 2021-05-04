module KMonad.App.Signal.Example

where

import KMonad.Prelude
import KMonad.Prelude.Example

import KMonad.Core
import KMonad.App.Types
import KMonad.App.Signal


--------------------------------------------------------------------------------
-- $example for `asList`


-- | NOTE: You cannot unlines the list, because that is strict, and blocks. You
-- can print the list out, because that's lazy, and happens as it happens.
behold :: UIO m => m ()
behold = metronome 1 >>= flip asList printList

-- | As you can see, within the `asList` context, `as` functions entirely as a
-- normal list. If you ever access unresolved elements, computation will just
-- pause until they are available.
get3d :: UIO m => m ()
get3d = metronome 1 >>= \tick -> do
  print $ "Starting to try to get the 3d element, should take about 3s"
  asList tick $ \as -> do
    print $ as !! 2
    print $ "Now accessing the first 2 elements, should be immediate"
    print $ take 2 as
    print $ "Now accessing the 4th element, should take about 1s"
    print $ as !! 3
