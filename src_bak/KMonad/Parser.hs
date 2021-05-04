module KMonad.Parser

where

import KMonad.App

loadCfg :: (EnvIO m env, HasLogEnv env) => FilePath -> m AppCfg
loadCfg _ = do
  le <- view logEnv
  pure $ AppCfg le
