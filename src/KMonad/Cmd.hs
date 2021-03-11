{-|

Module      : KMonad.Cmd
Description : The entry-point of KMonad
Copyright   : (c) David Janssen, 2021
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable

-}

module KMonad.Cmd
  ( Cmd(..)
  , getCmd
  , Task(..)
  , HasRunOps(..)
  )
where

import KMonad.App

import Options.Applicative as O

--------------------------------------------------------------------------------
-- $types


-- | Collection of different tasks kmonad can be instructed to do
data Task =
  Run RunOps -- ^ Run kmonad with the provided options

-- | All the configurable options to run kmonad with
data RunOps = RunOps
  { _cfgEntry :: !FilePath -- ^ Where to start reading the configuration
  }
 

-- | The instruction passed to kmonad at launch
data Cmd = Cmd
  { _task    :: Task   -- ^ What we are supposed to do
  , _cLogCfg :: LogCfg -- ^ How to configure logging
  }
makeClassy ''Cmd
instance HasLogCfg Cmd where logCfg = cLogCfg


-- Make and hookup some lenses
makeClassy ''RunOps

-- | Command to call to parse kmonad's invocation
getCmd :: MonadIO m => m Cmd
getCmd = liftIO
  $ customExecParser (prefs showHelpOnEmpty)
  $ info (fullP <**> helper)
    ( fullDesc
   <> progDesc "KMonad"
   <> header   "keymap manager" )

--------------------------------------------------------------------------------

-- | Top level parser
fullP :: Parser Cmd
fullP = Cmd <$> subP <*> logP

-- | Parse the sub-command
subP :: Parser Task
subP = subparser $ command "run" $ info runP idm

-- | Parse the run-command
runP :: Parser Task
runP = Run <$> (RunOps <$> fileP)

-- | Parse a config-file
fileP :: Parser FilePath
fileP = strArgument
  (  metavar "FILE"
  <> help    "Path to a config file")

-- | Parse logging config
--
-- TODO: finish me
logP :: Parser LogCfg
logP = LogCfg <$> loglevelP <*> pure stderr <*> pure defSep


-- | Parse a log-level
loglevelP :: Parser LogLevel
loglevelP = O.argument readm
  ( help "Level of log-messages to display"
 <> value LogInfo )
  where readm = maybeReader $ \case
          "info"  -> Just LogInfo
          "debug" -> Just LogDebug
          "error" -> Just LogError
          _       -> Nothing
