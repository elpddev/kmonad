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
  , withDefLog
  , LIO, LUIO
  , section
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

defLogCfg :: LogCfg
defLogCfg = LogCfg
  { _logLvl = LogDebug
  , _logTgt = stdout
  , _logSep = defSep
  }

-- | The logging runtime env
data LogEnv = LogEnv
  { _leLogCfg  :: !LogCfg  -- ^ Copy of the config
  , _leLogFunc :: !LogFunc -- ^ RIO LogFunc
  }
makeClassy ''LogEnv

instance HasCfg     LogEnv LogCfg where cfg      = leLogCfg
instance HasLogFunc LogEnv        where logFuncL = leLogFunc


type LIO m env = (EnvIO m env, HasLogEnv env, HasLogCfg env)
type LUIO m env = (EnvIO m env, HasLogEnv env)

-- | Use 'LogCfg' to run a context-continuation
withLog :: UIO m => LogCfg -> (LogEnv -> m a) -> m a
withLog c f = do
  -- Default to non-verbose logging
  raw <- logOptionsHandle (c^.logTgt) False
  let ops = raw & setLogMinLevel (c^.logLvl.asRIO)
  withLogFunc ops $ f . LogEnv c

-- | Run with default logging
withDefLog :: UIO m => (LogEnv -> m a) -> m a
withDefLog = withLog defLogCfg


-- | 'log_' and 'log' are used to display displayable things
log_ :: (LIO m env, Display a) => LogLevel -> a -> m ()
log_ l a = view logEnv >>= \env -> runRIO env $ f (display a)
  where f = case l of
          LogDebug -> logDebug
          LogInfo  -> logInfo
          LogError -> logError

-- | 'prt_' defaulted to 'LogDebug'
log :: (LIO m env, Display a) => a -> m ()
log = log_ LogDebug

-- | 'log_', but specialized to text for easier use code
say_ :: (LIO m env) => LogLevel -> Text -> m ()
say_ = log_

-- | 'log_', but specialized to text for easier use code
say :: (LIO m env) => Text -> m ()
say = log

-- | 'prt_' and 'prt' is used to display showable things
prt_ :: (LIO m env, Show a) => LogLevel -> a -> m ()
prt_ l = log_ l . pack . show

-- | 'prt_' defaulted to 'LogDebug'
prt :: (LIO m env, Show a) => a -> m ()
prt = log_ LogDebug . pack . show

section_ :: (LIO m env) => LogLevel -> m ()
section_ l = view logSep >>= \case
  Nothing -> pure ()
  Just t  -> log_ l t

section :: (LIO m env) => m ()
section = section_ LogDebug

--------------------------------------------------------------------------------
