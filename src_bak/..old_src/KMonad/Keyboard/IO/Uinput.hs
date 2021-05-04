{-# LANGUAGE DeriveAnyClass #-}
module KMonad.Keyboard.IO.Uinput

where

import KMonad.Prelude

import Foreign.C.Types
import Foreign.C.String
import System.Posix
import UnliftIO.Process (callCommand)

import KMonad.Keyboard.Linux.Types

--------------------------------------------------------------------------------
-- $cfg
--
-- In Linux we use the uinput sub-system to transmit keyboard events to the OS.

-- | How to configure `uinput`
data Ucfg = Ucfg
  { _vendorCode     :: !CInt           -- ^ USB Vendor code
  , _productCode    :: !CInt           -- ^ USB Product code
  , _productVersion :: !CInt           -- ^ USB Product version
  , _keyboardName   :: !String         -- ^ Name to give to the keyboard
  , _postInit       :: !(Maybe String) -- ^ Command to run after keyboard is made
  } deriving (Eq, Show)
makeClassy ''Ucfg

-- | Default Uinput configuration
defUinputCfg :: Ucfg
defUinputCfg = Ucfg
  { _vendorCode     = 0x1235
  , _productCode    = 0x5679
  , _productVersion = 0x0000
  , _keyboardName   = "KMonad simulated keyboard"
  , _postInit       = Nothing
  }

-- | Environment for handling uinput operations
data Uenv = Uenv
  { _ucfg' :: Ucfg    -- ^ The configuration of this uinput device
  , _dev   :: MVar Fd -- ^ MVar to the filehandle of the device
  }
makeClassy ''Uenv

-- | Hooking up some lenses
instance HasUcfg Uenv where ucfg = ucfg'

-- | Create a new Uenv from a Ucfg
mkUenv :: MonadIO m => Ucfg -> m Uenv
mkUenv c = Uenv c <$> newEmptyMVar

-- | The Uinput operations will need access to logging and the Uinput-environment
type CanUinput e = (HasLogFunc e, HasUenv e, HasUcfg e)


--------------------------------------------------------------------------------
-- $wrap
--
-- FFI calls to the C-code

foreign import ccall "acquire_uinput_keysink"
  acquire_uinput_keysink
    :: CInt    -- ^ Posix handle to the file to open
    -> CString -- ^ Name to give to the keyboard
    -> CInt    -- ^ Vendor ID
    -> CInt    -- ^ Product ID
    -> CInt    -- ^ Version ID
    -> IO Int

foreign import ccall "release_uinput_keysink"
  release_uinput_keysink :: CInt -> IO Int

foreign import ccall "send_event"
  send_event :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO Int


--------------------------------------------------------------------------------

-- | A collection of everything that can go wrong with the 'UinputSink'
data UinputSinkError
  = UinputRegistrationError String      -- ^ Could not register device
  | UinputReleaseError      String      -- ^ Could not release device
  deriving Exception

-- | How to display UinputSink errors
instance Show UinputSinkError where
  show (UinputRegistrationError snk)
    = "Could not register sink with OS: " <> snk
  show (UinputReleaseError snk)
    = "Could not unregister sink with OS: " <> snk
makeClassyPrisms ''UinputSinkError

--------------------------------------------------------------------------------
-- $ops

-- | Open a uinput device and register it with linux
open :: CanUinput e => RIO e ()
open = do

  logInfo "Opening '/dev/uinput'"
  fd@(Fd h) <- liftIO . openFd "/dev/uinput" WriteOnly Nothing $
    OpenFileFlags False False False True False

  name    <- view keyboardName
  product <- view productCode
  vendor  <- view vendorCode
  version <- view productVersion
  logInfo $ "Registering uinput device: " <> displayShow name
  liftIO $ do
    withCString name $ \s ->
      acquire_uinput_keysink h s vendor product version
        `onErr` UinputRegistrationError name

  view postInit >>= \case
    Nothing -> pure ()
    Just cmd -> do
      logInfo $ "Running uinput post-init command: " <> displayShow cmd
      void . async . callCommand $ cmd

  d <- view dev
  putMVar d fd

-- | Unregister a uinput device with linux and close the file
close :: CanUinput e => RIO e ()
close = do
  fd@(Fd h) <- takeMVar =<< view dev
  name      <- view keyboardName

  let release = do
        logInfo $ "Unregistering Uinput device: " <> displayShow name
        liftIO $ release_uinput_keysink h
                   `onErr` UinputReleaseError name

  let close = do
        logInfo $ "Closing Uinput device file for: " <> displayShow name
        liftIO . closeFd $ fd

  finally release close

-- | Write a keyboard event to the sink and sync the driver state.
write :: CanUinput e => LE -> RIO e ()
write e = do
  d    <- view dev
  withMVar d $ \(Fd h) -> do
    sendOne h $ _LRaw # e
    sendOne h =<< now sync
  where
    sendOne h LRaw{_leType=t, _leCode=l, _leVal=v, _leS=s, _leNS=ns} =
      void . liftIO $ send_event h (fi t) (fi l) (fi v) (fi s) (fi ns)
