{-|

Module      : KMonad.App.Logging
Description : The logging capabilities of KMonad
Copyright   : (c) David Janssen, 2021
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable

-}

module KMonad.App.Logging
  ( withLog
  , LogLevel(..)
  , LogCfg(..)
  , LogEnv
  , HasLogCfg(..)
  , HasLogEnv(..)
  , log_, log, say_, say, prt_, prt
  , defSep
  )
where

import KMonad.Core

import qualified RIO as R (LogLevel(..))
import qualified RIO.Text as T

-- | Default section seperator
defSep :: Maybe Text
defSep = Just $ T.replicate 80 "-"

-- | The log-levels we support
data LogLevel
  = LogDebug  -- ^ Write full reports
  | LogInfo   -- ^ Write only status updates
  | LogError  -- ^ Only write errors
  deriving Show

asRIO :: Getter LogLevel R.LogLevel
asRIO = to $ \case
  LogDebug -> R.LevelDebug
  LogInfo  -> R.LevelInfo
  LogError -> R.LevelError

-- | The full logging configuration provided by "KMonad.Cmd.getCmd"
data LogCfg = LogCfg
  { _logLvl :: !LogLevel     -- ^ What level to log at
  , _logTgt :: !Handle       -- ^ Where to log to
  , _logSep :: !(Maybe Text) -- ^ If and what to use as section separator
  } deriving Show
makeClassy ''LogCfg

-- | The logging runtime env
data LogEnv = LogEnv
  { _leLogCfg  :: !LogCfg  -- ^ Copy of the config
  , _leLogFunc :: !LogFunc -- ^ RIO LogFunc
  }
makeClassy ''LogEnv

instance HasCfg     LogEnv LogCfg where cfg      = leLogCfg
instance HasLogFunc LogEnv        where logFuncL = leLogFunc


-- | Use 'LogCfg' to run a context-continuation
withLog :: MonadUnliftIO m => LogCfg -> (LogEnv -> m a) -> m a
withLog c f = do
  -- Default to non-verbose logging
  raw <- logOptionsHandle (c^.logTgt) False
  let ops = raw & setLogMinLevel (c^.logLvl.asRIO)
  withLogFunc ops $ f . LogEnv c

-- | 'log_' and 'log' are used to display displayable things
log_ :: (EnvIO m env, HasLogEnv env, Display a) => LogLevel -> a -> m ()
log_ l a = view logEnv >>= \env -> runRIO env $ f (display a)
  where f = case l of
          LogDebug -> logDebug
          LogInfo  -> logInfo
          LogError -> logError

-- | 'prt_' defaulted to 'LogDebug'
log :: (EnvIO m env, HasLogEnv env, Display a) => a -> m ()
log = log_ LogDebug

-- | 'log_', but specialized to text for easier use code
say_ :: (EnvIO m env, HasLogEnv env) => LogLevel -> Text -> m ()
say_ = log_

-- | 'log_', but specialized to text for easier use code
say :: (EnvIO m env, HasLogEnv env) => Text -> m ()
say = log

-- | 'prt_' and 'prt' is used to display showable things
prt_ :: (EnvIO m env, HasLogEnv env, Show a) => LogLevel -> a -> m ()
prt_ l = log_ l . pack . show

-- | 'prt_' defaulted to 'LogDebug'
prt :: (EnvIO m env, HasLogEnv env, Show a) => a -> m ()
prt = log_ LogDebug . pack . show
