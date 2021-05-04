{-# LANGUAGE CPP #-}
module KMonad.App.KeyIO
  ( module X )
where

import KMonad.App.KeyIO.Types as X
import KMonad.App.KeyIO.Operations as X
import KMonad.App.KeyIO.Test as X

#ifdef linux_HOST_OS
import KMonad.App.KeyIO.Linux as X
#endif

#ifdef mingw32_HOST_OS
import KMonad.App.KeyIO.Windows as X
#endif

#ifdef darwin_HOST_OS
import KMonad.App.KeyIO.Mac as X
#endif
