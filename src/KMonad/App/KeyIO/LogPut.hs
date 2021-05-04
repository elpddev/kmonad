module KMonad.App.KeyIO.LogPut

where

import KMonad.Prelude
import KMonad.App.Logging
import KMonad.App.KeyIO.Types

data LogPutCfg = LogPutCfg

instance Default LogPutCfg where
  def = LogPutCfg

data LogPutEnv = LogPutEnv
  { _lpLogEnv :: LogEnv}
makeClassy ''LogPutEnv

instance HasLogEnv LogPutEnv where logEnv = lpLogEnv

withLogPut :: (LUIO m env) => With LogPutCfg PutKey m a
withLogPut _ f = do
  let init = do
        le <- view logEnv
        pure $ LogPutEnv le

  let cleanup _ = pure ()

  let go env = f (runRIO env . print)

  bracket init cleanup go
