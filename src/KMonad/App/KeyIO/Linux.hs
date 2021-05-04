module KMonad.App.KeyIO.Linux
  ( withGetKeyIO
  , withPutKeyIO
  , module X
  )
where

import KMonad.Prelude
import KMonad.Object.Context

import KMonad.App.Logging
import KMonad.App.KeyIO.Linux.Types  as X
import KMonad.App.KeyIO.Linux.Evdev  as X
import KMonad.App.KeyIO.Linux.Uinput as X


withGetKeyIO :: (LUIO m env) => IOGetCfg -> Ctx r m GetKey
withGetKeyIO (EvdevGet c) = withEvdev c

withPutKeyIO :: (LUIO m env) => IOPutCfg -> Ctx r m PutKey
withPutKeyIO (UinputPut c) = withUinput c


-- NOTE: This module will only get imported if we are actually in Linux
-- It must provide:
--
-- 1. Keycode type with instances for: Eq, Show
-- 2. keyNames hashmap from Keyname to Keycode
-- 3. keyAliases hashmap from Keyname to Keyname
-- 4. loadGetToken and loadPutToken functions

