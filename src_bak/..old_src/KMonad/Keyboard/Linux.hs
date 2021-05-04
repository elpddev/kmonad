module KMonad.Keyboard.Linux

where

import KMonad.Prelude
import KMonad.Keyboard.Linux.Types
import KMonad.Keyboard.Linux.Uinput
import KMonad.Keyboard.Linux.Evdev

data LinuxEnv = LinuxEnv
  { _ui :: UinputCfg
  , _ev :: EvdevCfg
  , _lf :: LogFunc
  }
makeClassy ''LinuxEnv

type Linux = RIO LinuxEnv

-- instance HasUinputCfg LinuxEnv where uinputCfg = uinput.uinputCfg
-- instance HasLogFunc   LinuxEnv where logFuncL  = logFunc

-- mkLinuxEnv :: HasLogFunc e => UinputCfg -> RIO e LinuxEnv
-- mkLinuxEnv ucfg = do
--   lf   <- view logFuncL
--   pure $ LinuxEnv ucfg () lf

-- type Linux = RIO LinuxEnv

instance (MonadUnliftIO m) => CanKIO Linux m where
  type Keycode Linux = LinuxCode
  withKIO f = do
    ucfg <- view ui
    ecfg <- view ev
    lf   <- view lf

    -- let g = withEvdev ecfg lf f
    -- pure $ withUinput ucfg lf g
    -- pure $ withUinput ucfg lf $ withEvdev ecfg lf $ f
    pure $ do
      g <- withEvdev ecfg lf f
      withUinput ucfg lf g
-- withUinput

-- instance CanKIO Linux where
--   type Keycode Linux = LinuxCode
--   withKIO f = do
--     ucfg <- view ui
--     withUinput ucfg . withEvdev $ f
  -- runOS  = uinputRun

-- withUinput :: MonadUnliftIO m => (PutKey Linux m -> a) -> a
-- withUinput f = undefined

-- withLinux ::

linuxRun :: (HasLogFunc e, HasLinuxEnv e) => RIO e a -> RIO e a
linuxRun = do
  undefined
