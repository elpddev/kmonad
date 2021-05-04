module KMonad.App.Invocation.Types
 
where

import KMonad.Prelude

import KMonad.App.Logging.Types


--------------------------------------------------------------------------------
-- $task

-- | The collection of different things _kmonad can be instructed to do
data Task
  = Run RunTaskCfg
  deriving Show

class HasCfgPath a where cfgPath :: Getter a FilePath

--------------------------------------------------------------------------------

-- | All the configuration options that can be passed to the run command
data RunTaskCfg = RunTaskCfg
  { _rCfgPath :: !FilePath -- ^ Where to start reading the configuration
  } deriving Show
makeClassy ''RunTaskCfg

instance HasCfgPath RunTaskCfg where
  cfgPath = rCfgPath

instance Default RunTaskCfg where
  def = RunTaskCfg "~/.config/kmonad"

--------------------------------------------------------------------------------
-- $invoc

-- | An invocation is what was passed to _kmonad at launch.
data Invocation = Invocation
  { _task   :: !Task    -- ^ What we should be doing
  , _cLogCfg :: !LogCfg -- ^ How we should report things
  } deriving Show
makeClassy ''Invocation

instance HasLogCfg Invocation where logCfg = cLogCfg

instance Default Invocation where
  def = Invocation (Run def) def
