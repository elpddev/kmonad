module KMonad.App.OS.Linux.IO

where

import KMonad.Prelude

import KMonad.App.KeyIO
import KMonad.App.Logging
import KMonad.App.OS.Types
import KMonad.App.OS.Linux.Types

notOnLinux :: WrongOSError
notOnLinux = WrongOSError "linux"

linLoadGetKey :: (LUIO m env)
  => GetToken -> m (WithGetKey Lcode)
linLoadGetKey (ListGet s) = undefined
linLoadGetKey (EvdevGet)  = say "Loading evdev" >> undefined
linLoadGetKey _           = throw notOnLinux

linLoadPutKey :: (LUIO m env)
  => PutToken -> m (WithPutKey Lcode)
linLoadPutKey LogPut = undefined
linLoadPutKey UinPut = say "Loading uinput" >> undefined
linLoadPutKey _      = throw notOnLinux
