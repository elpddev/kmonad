{-# LANGUAGE DeriveAnyClass #-}
{-|
Module      : KMonad.Keyboard.Linux.UinputSink
Description : Using Linux's uinput interface to emit events
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable
-}
module KMonad.Keyboard.Linux.UinputSink
  ( UinputSink
  , UinputCfg(..)
  , HasUinputCfg(..)
  -- , uinputSink
  , defUinputCfg
  )
where

import KMonad.Prelude

import Foreign.C.String
import Foreign.C.Types
import System.Posix
import UnliftIO.Async   (async)
import UnliftIO.Process (callCommand)

import KMonad.Keyboard.Linux.Types

--------------------------------------------------------------------------------
-- $err

-- type SinkId = String

-- -- | A collection of everything that can go wrong with the 'UinputSink'
-- data UinputSinkError
--   = UinputRegistrationError SinkId            -- ^ Could not register device
--   | UinputReleaseError      SinkId            -- ^ Could not release device
--   | SinkEncodeError         SinkId LRaw -- ^ Could not decode event
--   deriving Exception

-- -- | How to display UinputSink errors
-- instance Show UinputSinkError where
--   show (UinputRegistrationError snk) = "Could not register sink with OS: " <> snk
--   show (UinputReleaseError snk) = "Could not unregister sink with OS: " <> snk
--   show (SinkEncodeError snk a) = unwords
--     [ "Could not encode Keyaction"
--     , show a
--     , "to bytes for writing to"
--     , snk
--     ]
-- makeClassyPrisms ''UinputSinkError


--------------------------------------------------------------------------------
-- $cfg

-- | Configuration of the Uinput keyboard to instantiate
data UinputCfg = UinputCfg
  { _vendorCode     :: !CInt
  , _productCode    :: !CInt
  , _productVersion :: !CInt
  , _keyboardName   :: !String
  , _postInit       :: !(Maybe String)
  } deriving (Eq, Show)
makeClassy ''UinputCfg

-- | Default Uinput configuration
defUinputCfg :: UinputCfg
defUinputCfg = UinputCfg
  { _vendorCode     = 0x1235
  , _productCode    = 0x5679
  , _productVersion = 0x0000
  , _keyboardName   = "KMonad simulated keyboard"
  , _postInit       = Nothing
  }

-- | UinputSink is an MVar to a filehandle
data UinputSink = UinputSink
  { _cfg     :: UinputCfg
  , _st      :: MVar Fd
  }
makeLenses ''UinputSink



-- -- | Return a new uinput 'KeySink' with extra options
-- uinputSink :: HasLogFunc e => UinputCfg -> RIO e (Acquire KeySink)
-- uinputSink c = mkKeySink (usOpen c) usClose usWrite

--------------------------------------------------------------------------------
-- FFI calls and type-friendly wrappers

foreign import ccall "acquire_uinput_keysink"
  c_acquire_uinput_keysink
    :: CInt    -- ^ Posix handle to the file to open
    -> CString -- ^ Name to give to the keyboard
    -> CInt    -- ^ Vendor ID
    -> CInt    -- ^ Product ID
    -> CInt    -- ^ Version ID
    -> IO Int

foreign import ccall "release_uinput_keysink"
  c_release_uinput_keysink :: CInt -> IO Int

foreign import ccall "send_event"
  c_send_event :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO Int

-- | Create and acquire a Uinput device
acquire_uinput_keysink :: MonadIO m => Fd -> UinputCfg -> m Int
acquire_uinput_keysink (Fd h) c = liftIO $ do
  cstr <- newCString $ c^.keyboardName
  c_acquire_uinput_keysink h cstr
    (c^.vendorCode) (c^.productCode) (c^.productVersion)

-- | Release a Uinput device
release_uinput_keysink :: MonadIO m => Fd -> m Int
release_uinput_keysink (Fd h) = liftIO $ c_release_uinput_keysink h

-- | Using a Uinput device, send a LRaw to the Linux kernel
send_event :: MonadIO m => UinputSink -> Fd -> LRaw -> m ()
send_event u (Fd h) e = do
  (liftIO $ c_send_event h
   (fi $ e^.leType) (fi $ e^.leCode) (fi $ e^.leVal) (fi $ e^.leS) (fi $ e^.leNS))
    `onErr` SinkEncodeError (u^.cfg.keyboardName) e


--------------------------------------------------------------------------------

-- | Open and register a uinput keyboard
usOpen :: HasLogFunc e => UinputCfg -> RIO e UinputSink
usOpen c = do
  fd <- liftIO . openFd "/dev/uinput" WriteOnly Nothing $
    OpenFileFlags False False False True False
  logInfo "Registering Uinput device"
  acquire_uinput_keysink fd c `onErr` UinputRegistrationError (c ^. keyboardName)
  flip (maybe $ pure ()) (c^.postInit) $ \cmd -> do
    logInfo $ "Running UinputSink command: " <> displayShow cmd
    void . async . callCommand $ cmd
  UinputSink c <$> newMVar fd

-- | Close and unregister a uinput device
usClose :: HasLogFunc e => UinputSink -> RIO e ()
usClose snk = withMVar (snk^.st) $ \h -> finally (release h) (close h)
  where
    release h = do
      logInfo $ "Unregistering Uinput device"
      release_uinput_keysink h
        `onErr` UinputReleaseError (snk^.cfg.keyboardName)

    close h = do
      logInfo $ "Closing Uinput device file"
      liftIO $ closeFd h

-- | Write a keyboard event to the sink and sync the driver state.
usWrite :: HasLogFunc e => UinputSink -> LE -> RIO e ()
usWrite u e = withMVar (u^.st) $ \fd -> do
  send_event u fd $ _LRaw # e
  send_event u fd =<< now sync
