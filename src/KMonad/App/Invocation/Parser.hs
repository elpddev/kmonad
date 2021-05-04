module KMonad.App.Invocation.Parser
  ( main )
where

import KMonad.Prelude hiding (Parser)
import KMonad.App.Invocation.Types hiding (task)
import KMonad.App.Logging.Types
import Options.Applicative as O


-- | Top level parser
main :: Parser Invocation
main = Invocation <$> task <*> log

-- | Task-parsing subparser
task :: Parser Task
task = subparser $ command "run" $ info run idm

-- | Parse a run-command
run :: Parser Task
run = Run <$> (RunTaskCfg <$> path)

-- | Parse a config-file
path :: Parser FilePath
path = strArgument
  (  metavar "FILE"
  <> help    "Path to a config file")

-- | Parse logging config
--
-- TODO: finish me
log :: Parser LogCfg
log = def `setTo` logLvl <$> loglevel

-- | Parse a log-level
loglevel :: Parser LogLevel
loglevel = O.option readm
  ( help "Level of log-messages to display"
 <> short 'l'
 <> long  "loglevel"
 <> value LevelInfo )
  where readm = maybeReader $ \case
          "info"  -> Just LevelInfo
          "debug" -> Just LevelDebug
          "error" -> Just LevelError
          _       -> Nothing
