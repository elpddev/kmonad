module KMonad.App.KeyIO.Test

where

import KMonad.Prelude
import KMonad.Object.Context
import KMonad.App.Logging
import KMonad.App.KeyIO.Types
import KMonad.App.KeyIO.Test.ListGet
import KMonad.App.KeyIO.Test.LogPut
import KMonad.App.Parser (keycode)

withGetKeyTest :: LUIO m env => TestGetCfg -> Ctx r m GetKey
withGetKeyTest (ListGet t) = do
  let es = parseThrow (some keycode) t
  withListGet $ ListGetCfg es

withPutKeyTest :: LUIO m env => TestPutCfg -> Ctx r m PutKey
withPutKeyTest LogPut = withLogPut LogPutCfg
