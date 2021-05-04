{-|

Module      : KMonad.Prelude
Description : Code that we assume is available everywhere
Copyright   : (c) David Janssen, 2021
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable

We try to stick close to the RIO prelude, with a few alterations:
- Prefer Control.Lens over RIO support
- Some basic stuff from RIO available at top-level
- Hide logging so we can shadow with our own

This is also where we define some simple utility functions that don't have a
special place to live.

-}
module KMonad.Prelude
  ( -- * Reexports
    module X

  )
where

import KMonad.Prelude.Util         as X
import KMonad.Types                as X

import Control.Lens                as X
import RIO.Text                    as X (unlines, lines, pack, unpack)

import RIO as X hiding
  (
    -- Prefer "Control.Lens"
    view, ASetter, ASetter', Lens, Getting, Lens'
  , SimpleGetter, lens, over, set, sets, to, (^.)
  , (^..), (^?), preview, (%~), (.~)

    -- Some stuff I'd rather default to Text
  , unlines, lines

    -- Names we'd like available for our own things
  , log, LogLevel

    -- Changed to a monad-constraint
  , IO
  )
