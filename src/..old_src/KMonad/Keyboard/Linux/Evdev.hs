{-# LANGUAGE DeriveAnyClass #-}
{-|
Module      : KMonad.Keyboard.Linux.Evdev
Description : Load and acquire a linux /dev/input device
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable
-}
module KMonad.Keyboard.Linux.Evdev
  ( EvdevCfg
  , HasEvdevCfg(..)
  , withEvdev
  )
where

import KMonad.Prelude

import Foreign.C.Types
import System.Posix

import KMonad.Keyboard.Linux.Types

import qualified Data.Serialize as B (decode)
import qualified RIO.ByteString as B


--------------------------------------------------------------------------------
-- $cfg
--
-- In Linux we use /dev/input files to read capture keyboard events from the OS.

-- | Configurable components of a DeviceSource
data EvdevCfg = EvdevCfg
  { _pth     :: !FilePath -- ^ Path to the event-file
  }
makeClassy ''EvdevCfg

-- | Runtime environment for evdev-operations
data EvdevEnv = EvdevEnv
  { _cfg :: !EvdevCfg -- ^ Configuration settings
  , _lf  :: !LogFunc  -- ^ RIO logging function
  , _fd  :: !Fd       -- ^ Posix filedescriptor to the device file
  , _hdl :: !Handle   -- ^ Haskell handle to the device file
  }
makeClassy ''EvdevEnv

-- | Hooking up some lenses
instance HasEvdevCfg EvdevEnv where evdevCfg = cfg
instance HasLogFunc  EvdevEnv where logFuncL = lf


--------------------------------------------------------------------------------
-- $ffi

foreign import ccall "ioctl_keyboard"
  c_ioctl_keyboard :: CInt -> CInt -> IO CInt

-- | Perform an IOCTL operation on an open keyboard handle
ioctl_keyboard :: MonadIO m
  => Fd      -- ^ Descriptor to open keyboard file (like /dev/input/eventXX)
  -> Bool    -- ^ True to grab, False to ungrab
  -> m Int   -- ^ Return the exit code
ioctl_keyboard (Fd h) b = fi <$> liftIO (c_ioctl_keyboard h (bool 0 1 b))


--------------------------------------------------------------------------------
-- $decoding

decode :: B.ByteString -> Either String RawEvent
decode bs = f <$> r
  where
    r :: Either String (Int32, Word16, Word16, Word64, Word64)
    r = B.decode $ B.reverse bs
    f (a, b, c, d, e) = RawEvent e d c b a


-- | A 'KeyEventParser' describes how to read and parse 'LinuxKeyEvent's from
-- the binary data-stream provided by the device-file.
-- data KeyEventParser = KeyEventParser
--   { _nbytes :: !Int
--     -- ^ Size of 1 input event in bytes
--   , _prs    :: !(B.ByteString -> Either String LinuxEvent)
--     -- ^ Function to convert bytestring to event
--   }
-- makeClassy ''KeyEventParser

-- -- | Default configuration for parsing keyboard events
-- defEventParser :: KeyEventParser
-- defEventParser = KeyEventParser 24 decode64

-- -- | The KeyEventParser that works on my 64-bit Linux environment
-- decode64 :: B.ByteString -> Either String LinuxEvent
-- decode64 bs = mkEvent <$> result
--   where
--     result :: Either String (Int32, Word16, Word16, Word64, Word64)
--     result = B.decode . B.reverse $ bs

--     mkEvent (a, b, c, d, e) = LinuxEvent e d c b a


--------------------------------------------------------------------------------
-- $types


-- instance HasDeviceSourceCfg DeviceFile where deviceSourceCfg = cfg
-- instance HasKeyEventParser  DeviceFile where keyEventParser  = cfg.parser

-- -- | Open a device file
-- deviceSource :: HasLogFunc e
--   => KeyEventParser -- ^ The method by which to read and decode events
--   -> FilePath    -- ^ The filepath to the device file
--   -> RIO e (Acquire KeySource)
-- deviceSource pr pt = mkKeySource (lsOpen pr pt) lsClose lsRead

-- -- | Open a device file on a standard linux 64 bit architecture
-- deviceSource64 :: HasLogFunc e
--   => FilePath  -- ^ The filepath to the device file
--   -> RIO e (Acquire KeySource)
-- deviceSource64 = deviceSource defEventParser

--------------------------------------------------------------------------------
-- $err

data DeviceSourceError
  = IOCtlGrabError    FilePath
  | IOCtlReleaseError FilePath
  | KeyIODecodeError  String
  deriving Exception

instance Show DeviceSourceError where
  show (IOCtlGrabError pth)    = "Could not perform IOCTL grab on: "    <> pth
  show (IOCtlReleaseError pth) = "Could not perform IOCTL release on: " <> pth
  show (KeyIODecodeError msg)  = "KeyEvent decode failed with msg: "    <> msg

makeClassyPrisms ''DeviceSourceError

--------------------------------------------------------------------------------
-- $io

evdevOpen :: MonadIO m => LogFunc -> EvdevCfg -> m EvdevEnv
evdevOpen lf c = runRIO lf $ do
 
  logInfo $ "Opening '" <> displayShow (c^.pth) <> "'"
  fd  <- liftIO . openFd (c^.pth) ReadOnly Nothing $
    OpenFileFlags False False False False False
  h <- liftIO $ fdToHandle fd

  logInfo $ "Acquiring ioctl grab"
  ioctl_keyboard fd True `onErr` IOCtlGrabError (c^.pth)

  pure $ EvdevEnv c lf fd h


evdevClose :: RIO EvdevEnv ()
evdevClose = ask >>= \c -> do
  logInfo $ "Releasing ioctl grab"
  ioctl_keyboard (c^.fd) False `onErr` IOCtlReleaseError (c^.pth)
  logInfo $ "Closing '" <> displayShow (c^.pth) <> "'"
  liftIO . closeFd $ c^.fd

evdevRead :: RIO EvdevEnv LinuxEvent
evdevRead = ask >>= \c -> do
  bts <- B.hGet (c^.hdl) 24 -- 24 bytes is the length of 1 evdev event
  case decode bts of
    Right p -> case p ^? _RawEvent of
      Just e  -> pure e
      Nothing -> evdevRead                 -- Decodeable but irrelevant event
    Left s -> throwIO $ KeyIODecodeError s -- Undecodeable event (shouldn't happen?)


withEvdev :: MonadUnliftIO m
  => EvdevCfg
  -> LogFunc
  -> (m LinuxEvent -> m a)
  -> m a
withEvdev c lf f = do
  bracket (evdevOpen lf c) (flip runRIO evdevClose)
    (\env -> f $ runRIO env evdevRead)
