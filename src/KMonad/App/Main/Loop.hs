module KMonad.App.Main.Loop

where

import KMonad.Prelude

import KMonad.App.Logging
import KMonad.App.Invocation
import KMonad.App.KeyIO
import KMonad.App.Main.Types

import KMonad.Object.Context
import KMonad.Object.Stream

withEvents :: (LUIO m env) => RunCfg -> Ctx r m (Stream KeyEvent)
withEvents r = do
  lift $ say_ LevelInfo "Initializing key-io"
  kio <- withKio r
  lift $ say_ LevelInfo "Starting app-loop"
  withStreamIO (runRIO kio getKey)




loop :: RunCfg -> OnlyLIO ()
loop r = runCtx (withEvents r) $ \es -> mapM_ print es
-- do
--   say_ LevelInfo "Initializing key-io"
--   runCtx (withKio r) $ \kio -> do
--     runRIO kio $ withStreamIO getKey $ \es -> do
--       mapM_ print es
