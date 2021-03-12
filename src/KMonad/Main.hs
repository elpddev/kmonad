{-|

Module      : KMonad.Main
Description : The entry-point of KMonad
Copyright   : (c) David Janssen, 2021
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable

This module is the entry-point for kmonad. The @app/Main.hs@ module imports the
'main' function from this module (and nothing else).

The first thing we always do is configure and initialize logging. After that we
run whatever command was provided in the invocation of kmonad.

-}

module KMonad.Main
  ( main )
where

import KMonad.Prelude

import KMonad.Cmd
import KMonad.App


-- | The entry-point for kmonad
--
-- 1. Runs the command-line invocation parser
-- 2. Initializes the logging environment
-- 3. Runs the task-dispatcher
main :: IO ()
main = getCmd >>= \(Cmd tsk lcfg) -> do
  let logcfg = (lcfg & logLvl .~ LogDebug) -- FIXME: this shouldn't default
  withLog logcfg $ flip runRIO $ runTask tsk

-- | Perform the task indicated by the command.
--
-- Currently supported tasks:
-- - Run: Parse a configuration and run the kmonad remapper
runTask :: (EnvUIO m env, HasLogEnv env) => Task -> m ()
runTask (Run ops) = do
  le <- view logEnv
  let appCfg = AppCfg le
  withApp appCfg $ flip runRIO $ loop
