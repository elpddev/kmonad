{-# LANGUAGE TypeFamilyDependencies #-}

module KMonad.App.OS.Types

where


import KMonad.Prelude

import KMonad.App.KeyIO
import KMonad.App.Logging
import System.Info (os)

class CanKeycode (Keycode os) => CanOS os where

  type Keycode (os :: *) = e | e -> os

  loadGetKey :: LUIO m env => os -> GetToken -> m (WithGetKey (Keycode os))
  loadPutKey :: LUIO m env => os -> PutToken -> m (WithPutKey (Keycode os))

data WrongOSError = WrongOSError String
instance Show WrongOSError where
  show (WrongOSError os) = "Not supported under: " <> os
instance Exception WrongOSError

