module KMonad.App.Main
  ( main )
where

import KMonad.Prelude

import KMonad.App.Logging
import KMonad.App.Invocation
import KMonad.App.Main.Loop
import KMonad.App.Main.Types
import KMonad.App.CfgFiles

-- | Get the command-line and run it
main :: OnlyIO ()
main = getInvocation >>= runInvocation

-- | Setup logging and dispatch on the task
runInvocation :: Invocation -> OnlyIO ()
runInvocation invoc = runLog (invoc ^. logCfg) $ case (invoc ^. task) of

  -- Run _kmonad's main keyboard remapping process
  Run c -> do
    print invoc >> section
    rcfg <- loadCfg $ c^.cfgPath
    loop rcfg

--------------------------------------------------------------------------------
-- Testing stuff

-- | Run the default invocation for interactive testing
runTest :: OnlyIO ()
runTest = runInvocation def

