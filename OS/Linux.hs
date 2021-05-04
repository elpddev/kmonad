{-# LANGUAGE CPP #-}
module KMonad.App.OS.Linux
  ( module X
  , Linux(..)
#ifdef linux_HOST_OS
  , currentOS
#endif
  )
where

import KMonad.Prelude

import KMonad.App.OS.Linux.Types as X
import KMonad.App.OS.Linux.IO    as X
import KMonad.App.OS.Types

#ifdef linux_HOST_OS
import KMonad.App.OS.Linux.LinuxOnly

currentOS :: Linux
currentOS = Linux
#endif

data Linux = Linux deriving (Eq, Show)

instance CanOS Linux where
  type Keycode Linux = Lcode

  loadGetKey _ = linLoadGetKey
  loadPutKey _ = linLoadPutKey
