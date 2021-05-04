{-# LANGUAGE DeriveAnyClass #-}
module KMonad.Keyboard.Linux.Uinput
  ( UinputCfg(..)
  , HasUinputCfg(..)
  , defUinputCfg

  , withUinput
  )
where

import KMonad.Prelude hiding (product)

import Foreign.C.Types
import Foreign.C.String
import System.Posix hiding (version)
import UnliftIO.Process (callCommand)

import KMonad.Keyboard.Linux.Types

--------------------------------------------------------------------------------
-- $cfg
--
-- In Linux we use the uinput sub-system to transmit keyboard events to the OS.

-- | How to configure `uinput`
data UinputCfg = UinputCfg
  { _vendor   :: !CInt           -- ^ USB Vendor code
  , _product  :: !CInt           -- ^ USB Product code
  , _version  :: !CInt           -- ^ USB Product version
  , _name     :: !String         -- ^ Name to give to the keyboard
  , _postInit :: !(Maybe String) -- ^ Command to run after keyboard is made
  } deriving (Eq, Show)
makeClassy ''UinputCfg

-- | Default Uinput configuration
defUinputCfg :: UinputCfg
defUinputCfg = UinputCfg
  { _vendor   = 0x1234
  , _product  = 0x5678
  , _version  = 0x0000
  , _name     = "KMonad simulated keyboard"
  , _postInit = Nothing
  }

-- | Environment for handling uinput operations
data UinputEnv = UinputEnv
  { _uinputCfg' :: UinputCfg -- ^ The configuration of this uinput device
  , _logFunc    :: LogFunc   -- ^ RIO logging function
  , _dev        :: MVar Fd   -- ^ MVar to the filehandle of the device
  }
makeClassy ''UinputEnv

-- | Hooking up some lenses
instance HasUinputCfg UinputEnv where uinputCfg = uinputCfg'
instance HasLogFunc   UinputEnv where logFuncL  = logFunc

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
uinputOpen :: MonadIO m => LogFunc -> UinputCfg -> m UinputEnv
uinputOpen lf c = runRIO lf $ do

  logInfo "Opening '/dev/uinput'"
  fd@(Fd h) <- liftIO . openFd "/dev/uinput" WriteOnly Nothing $
    OpenFileFlags False False False True False

  logInfo $ "Registering uinput device: " <> displayShow (c^.name)
  liftIO $ do
    withCString (c^.name) $ \s ->
      acquire_uinput_keysink h s (c^.vendor) (c^.product) (c^.version)
        `onErr` UinputRegistrationError (c^.name)

  flip (maybe $ pure ()) (c^.postInit) $ \cmd -> do
    logInfo $ "Running uinput post-init command: " <> displayShow cmd
    void . async . callCommand $ cmd

  UinputEnv c lf <$> newMVar fd

-- | Unregister a uinput device with linux and close the file
uinputClose :: RIO UinputEnv ()
uinputClose = do
  fd@(Fd h) <- takeMVar =<< view dev
  nm        <- view name

  let release = do
        logInfo $ "Unregistering Uinput device: " <> displayShow nm
        liftIO $ release_uinput_keysink h
                   `onErr` UinputReleaseError nm

  let close = do
        logInfo $ "Closing Uinput device file for: " <> displayShow nm
        liftIO . closeFd $ fd

  finally release close

-- | Write a keyboard event to the sink and sync the driver state.
uinputWrite :: LinuxEvent -> RIO UinputEnv ()
uinputWrite e = do
  d    <- view dev
  withMVar d $ \(Fd h) -> do
    sendOne h $ _RawEvent # e
    sendOne h =<< now sync
  where
    sendOne h RawEvent{_leType=t, _leCode=l, _leVal=v, _leS=s, _leNS=ns} =
      void . liftIO $ send_event h (fi t) (fi l) (fi v) (fi s) (fi ns)

-- | Context handler for Uinput writing
withUinput :: MonadUnliftIO m
  => UinputCfg                             -- ^ Uinput device settings
  -> LogFunc
  -> ((LinuxEvent -> m ()) -> m a) -- ^ Continuation to be filled in with writer
  -> m a                               -- ^ Action that uses uinput as writer
withUinput cfg lf f = do
  bracket (uinputOpen lf cfg) (flip runRIO uinputClose)
    (\env -> f $ \e -> runRIO env (uinputWrite e))
