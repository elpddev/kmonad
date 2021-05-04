{-# LANGUAGE CPP #-}
module KMonad.App.KeyIO.Types
  ( TestGetCfg(..)
  , TestPutCfg(..)
  , IOGetCfg(..)
  , IOPutCfg(..)
  , KeyEvent(..)
  , HasKeyEvent(..)
  , KioCfg(..)
  , HasKioCfg(..)
  , KioEnv(..)
  , HasKioEnv(..)
  , KIO
  , KUIO
  , module T)
where

import KMonad.Prelude

import KMonad.App.Logging
import KMonad.Object.Name
import KMonad.Object.Stream
import KMonad.Object.Time

#ifdef linux_HOST_OS
import KMonad.App.KeyIO.Linux as T
#endif

#ifdef mingw32_HOST_OS
import KMonad.App.KeyIO.Windows.Types as T
#endif

#ifdef darwin_HOST_OS
import KMonad.App.KeyIO.Mac.Types as T
#endif

import KMonad.App.KeyIO.Linux.Common as T
import KMonad.App.KeyIO.Windows.Common as T
import KMonad.App.KeyIO.Mac.Common as T

import qualified RIO.HashMap as M

--------------------------------------------------------------------------------
-- $event
--
-- KeyEvents are a combination of the triggering keycode with the name of the
-- source element, and a _kmonad-generated timestamp.

data KeyEvent = KeyEvent
  { _src  :: Name
  , _code :: Keycode
  , _t    :: Time
  } deriving Show
makeClassy ''KeyEvent


instance HasTime KeyEvent where time = t

--------------------------------------------------------------------------------
-- $cfg
--
-- The full configuration of I and O is a non-empty list of a collection of
-- tokens.

data KioCfg = KioCfg
  { _getKeyCfg :: NameMap GetCfg
  , _putKeyCfg :: NameMap PutCfg
  }
makeClassy ''KioCfg

mykb :: FilePath
mykb = "/dev/input/by-id/usb-04d9_daskeyboard-event-kbd"

instance Default KioCfg where
    def = KioCfg
      { _getKeyCfg = M.fromList [("main", Right . EvdevGet $ EvdevCfg  mykb)]
      , _putKeyCfg = M.fromList [ ("uinput", Right $ UinputPut def)
                              --, ("log", Left $ LogPut)
                                  ]}

-- | Environment in which KeyIO action are run
data KioEnv = KioEnv
  { _keCfg    :: KioCfg
  , _keLogEnv :: LogEnv
  , _getChan  :: Chan KeyEvent
  , _putters  :: [PutKey]
  }
makeClassy ''KioEnv

instance HasLogEnv KioEnv where logEnv = keLogEnv
instance HasKioCfg KioEnv where kioCfg = keCfg

--------------------------------------------------------------------------------

-- | The constraint of io, logging, and keyIO
type KIO m env = (LIO m env, HasKioEnv env)

-- | The constraint of uio, logging, and keyIO
type KUIO m env = (KIO m env, UIO m)
