module KMonad.App.Invocation.IO

where

import KMonad.Prelude

import KMonad.App.Invocation.Parser
import KMonad.App.Invocation.Types

import Options.Applicative as O

getInvocation :: IO m => m Invocation
getInvocation = liftIO
  $ customExecParser (prefs showHelpOnEmpty)
  $ info (main <**> helper)
    ( fullDesc
   <> progDesc "KMonad"
   <> header   "keymap manager" )
