module KMonad.Overview
  ()
where

import KMonad.Prelude

class Stream a where
  next :: m a
