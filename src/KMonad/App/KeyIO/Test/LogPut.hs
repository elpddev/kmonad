module KMonad.App.KeyIO.Test.LogPut

where

import KMonad.Prelude
import KMonad.Object.Context
import KMonad.App.Logging
import KMonad.App.KeyIO.Types

data LogPutCfg = LogPutCfg

instance Default LogPutCfg where
  def = LogPutCfg

data LogPutEnv = LogPutEnv
  { _lpLogEnv :: LogEnv}
makeClassy ''LogPutEnv

instance HasLogEnv LogPutEnv where logEnv = lpLogEnv

withLogPut :: (LUIO m env) => LogPutCfg -> Ctx r m PutKey
withLogPut _ = mkCtx $ \f -> do
  let init = do
        le <- view logEnv
        pure $ LogPutEnv le

  let cleanup _ = pure ()

  let go env = f (runRIO env . print)

  bracket init cleanup go
