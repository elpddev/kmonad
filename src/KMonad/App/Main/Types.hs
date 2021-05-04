module KMonad.App.Main.Types

where

import KMonad.Prelude

import KMonad.App.KeyIO
import qualified RIO.HashMap as M

-- placeholder
type ModelCfg = ()

data RunCfg = RunCfg
  { _rKioCfg   :: KioCfg
  , _rModelCfg :: ModelCfg
  }
makeClassy ''RunCfg

instance HasKioCfg RunCfg where kioCfg = rKioCfg

instance Default RunCfg where
  def = RunCfg def ()
    -- KioCfg
    -- { _getKeyCfg = M.fromList [("main", Right . EvdevGet $ EvdevCfg  mykb)]
    -- , _putKeyCfg = M.fromList [ ("uinput", Right $ UinputPut def)
    --                           --, ("log", Left $ LogPut)
    --                           ]}

data RunEnv = RunEnv
  { _rKioEnv :: KioEnv
  }
makeClassy ''RunEnv

instance HasKioEnv RunEnv where kioEnv = rKioEnv
