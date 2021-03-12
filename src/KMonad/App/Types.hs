{-# LANGUAGE RankNTypes #-}
module KMonad.App.Types

where

import KMonad.Core

import KMonad.App.Logging

data AppCfg = AppCfg
  { _acLogEnv :: LogEnv
  }
makeClassy ''AppCfg

-- instance HasLogEnv AppCfg where logEnv = acLogEnv


data AppEnv = AppEnv
  { _aeCfg :: AppCfg
  }
makeClassy ''AppEnv

instance HasCfg AppEnv AppCfg where cfg = aeCfg
instance HasLogEnv AppEnv where logEnv = aeCfg.acLogEnv

withApp :: MonadUnliftIO m => AppCfg -> (AppEnv -> m a) -> m a
withApp c f = f $ AppEnv c


type App a = forall m env. (EnvIO m env, HasLogEnv env, HasAppEnv env)  => m a

loop :: App ()
loop = say "looping'"
