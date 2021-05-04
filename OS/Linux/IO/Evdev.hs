module KMonad.App.OS.Linux.IO.Evdev

where

import KMonad.Prelude
import KMonad.App.Logging

import Foreign.C.Types
import System.Posix

import KMonad.App.OS.Linux.Types
import qualified RIO.Set as S

--------------------------------------------------------------------------------
-- $err

data EvdevError
  = IOCtlGrabError    FilePath
  | IOCtlReleaseError FilePath
makeClassyPrisms ''EvdevError

instance Show EvdevError where
  show (IOCtlGrabError pth)    = "Could not perform IOCTL grab on: "    <> pth
  show (IOCtlReleaseError pth) = "Could not perform IOCTL release on: " <> pth

instance Exception EvdevError

--------------------------------------------------------------------------------
-- $ffi
foreign import ccall "ioctl_keyboard"
  c_ioctl_keyboard :: CInt -> CInt -> OnlyIO CInt

-- | Perform an IOCTL operation on an open keyboard handle
ioctl_keyboard :: MonadIO m
  => Fd      -- ^ Descriptor to open keyboard file (like /dev/input/eventXX)
  -> Bool    -- ^ True to grab, False to ungrab
  -> m Int   -- ^ Return the exit code
ioctl_keyboard (Fd h) b = fromIntegral <$>
  liftIO (c_ioctl_keyboard h (if b then 1 else 0))

--------------------------------------------------------------------------------

data EvdevCfg = EvdevCfg
  { _pth :: FilePath }
makeClassy ''EvdevCfg

data EvdevEnv = EvdevEnv
  { _le     :: LogEnv
  , _cfg    :: EvdevCfg
  , _hdl    :: Handle
  , _fd     :: Fd
  , _active :: MVar (S.Set Lcode)
  }
makeClassy ''EvdevEnv

instance HasEvdevCfg EvdevEnv where evdevCfg = cfg
instance HasLogEnv   EvdevEnv where logEnv   = le

--------------------------------------------------------------------------------

nextEvent :: RIO EvdevEnv a
nextEvent = undefined

withEvdev :: LUIO m env => With EvdevCfg EvdevEnv m a
withEvdev c f = do
  let init = do
        le <- view logEnv
        -- Open the posix evdev file and convert to handle
        let flgs = OpenFileFlags False False False False False
        fd' <- liftIO $ openFd (c^.pth) ReadOnly Nothing flgs
        hdl' <- liftIO $ fdToHandle fd'

        -- Execute ioctl-grab
        say_ LevelInfo $ "Initiating ioctl grab"
        ioctl_keyboard fd' True `onErr` IOCtlGrabError (c^.pth)

        -- Wrap up the environment
        EvdevEnv le c hdl' fd' <$> newMVar S.empty

  let cleanup env = do
        say_ LevelInfo $ "Releasing ioctl grab"
        ioctl_keyboard (env^.fd) False `onErr` IOCtlReleaseError (env^.pth)
        liftIO . closeFd $ env^.fd

  -- FIXME CONTINUE HERE
  let go env = undefined
  -- let go env = f $ runRIO env nextEvent

  bracket init cleanup go
