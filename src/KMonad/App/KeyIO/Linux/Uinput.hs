module KMonad.App.KeyIO.Linux.Uinput
  ( withUinput )
where

import KMonad.Prelude
import KMonad.Object.Name
import KMonad.Object.Time
import KMonad.Object.Context
import KMonad.App.Logging

import Foreign.C.String
import Foreign.C.Types
import System.Posix
import UnliftIO.Process (callCommand)

import Data.Typeable
import UnliftIO.Exception hiding (throwIO)
import System.IO.Error.Lens
-- import Control.Exception.Safe (MonadCatch)

import KMonad.App.KeyIO.Linux.Types
import KMonad.App.KeyIO.Linux.Common

import qualified RIO.Set as S

--------------------------------------------------------------------------------
-- $cfg


-- | The environment used to handle uinput operations
data UinputEnv = UinputEnv
  { _cfg     :: UinputCfg            -- ^ The configuration with which we were started
  , _le      :: LogEnv               -- ^ The logging-env in which we were started
  , _kbf     :: Fd                   -- ^ Open file-descriptor to the uinput keyboard
  , _pressed :: MVar (S.Set Keycode) -- ^ Set of keys that are currently held
  }
makeClassy ''UinputEnv

instance HasUinputCfg UinputEnv where uinputCfg = cfg

--------------------------------------------------------------------------------
-- FFI calls and type-friendly wrappers

foreign import ccall "acquire_uinput_keysink"
  c_acquire_uinput_keysink
    :: CInt    -- ^ Posix handle to the file to open
    -> CString -- ^ Name to give to the keyboard
    -> CInt    -- ^ Vendor ID
    -> CInt    -- ^ Product ID
    -> CInt    -- ^ Version ID
    -> OnlyIO CInt

foreign import ccall "release_uinput_keysink"
  c_release_uinput_keysink :: CInt -> OnlyIO CInt

foreign import ccall "send_event"
  c_send_event :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> OnlyIO CInt

-- | Create and acquire a Uinput device
acquire_uinput_keysink :: MonadIO m => Fd -> UinputCfg -> m FFIResult
acquire_uinput_keysink (Fd h) c = liftIO $ do
  cstr <- newCString $ unpack $ c^.keyboardName
  ffiReturn <$> c_acquire_uinput_keysink h cstr
    (fi $ c^.vendorCode) (fi $ c^.productCode) (fi $ c^.productVersion)

-- | Release a Uinput device
release_uinput_keysink :: MonadIO m => Fd -> m FFIResult
release_uinput_keysink (Fd h) = liftIO $ ffiReturn <$> c_release_uinput_keysink h

-- | Using a Uinput device, send a RawEvent to the Linux kernel
send_event ::
     Fd
  -> RawEvent
  -> RIO e ()
send_event (Fd h) e = liftIO $ void $ c_send_event h
  (fi $ e^.leType) (fi $ e^.leCode) (fi $ e^.leVal) (fi $ e^.leS) (fi $ e^.leNS)


--------------------------------------------------------------------------------

-- | Send a keycode to the OS
--
-- NOTE: here we ensure that every odd keycode gets emitted as a press, and
-- every even keycode gets emitted as a release.
sendEvent :: Keycode -> RIO UinputEnv ()
sendEvent c = do
  h <- view kbf

  -- Send the event for the key we are trying to emit
  e <- overMVar (view pressed) $ \cs -> do
         if c `S.member` cs
           then (S.delete c cs,) <$> mkRaw False c
           else (S.insert c cs,) <$> mkRaw True  c
  send_event h e

  -- Send the sync-event to signal to linux to update the driver state
  e <- mkSync
  send_event h e


-- | How to manage the context of having a Uinput keyboard
withUinput :: (LUIO m env)
  => UinputCfg -> Ctx r m PutKey
withUinput c = mkCtx $ \f -> do
  maybeDo_ (c^.preInit) callCommand
  -- when (c^.doModprobe) $ callCommand "sudo modprobe uinput"
  -- How to display ffi-errors
  let showErr t n = t <> " '" <> c^.keyboardName
                      <> "' . With errorcode: " <> tshow n
  let init = do
        le <- view logEnv
        -- Open the file-handle to the standard uinput device
        fd <- liftIO $ openFd "/dev/uinput" WriteOnly Nothing $
                OpenFileFlags False False False True False

        -- Register the device-specs with the uinput kernel module
        say_ LevelInfo "Registering uinput device"
        acquire_uinput_keysink fd c `onErr` \n
          -> throwing _CouldNotAcquireKio $ showErr "Failed to acquire" n

        -- Optionally, fork of a command to be run
        maybeDo_ (c^.postInit) $ \cmd -> do
          say_ LevelInfo $ "Running post-uinput-init command: " <> (pack cmd)
          async . callCommand $ cmd

        UinputEnv c le fd <$> newMVar S.empty

  let cleanup env = do
        -- Unregister the device from the uinput kernel module
        say_ LevelInfo $ "Unregistering uinput device"
        let h = env^.kbf
        let rel = release_uinput_keysink h `onErr` \n ->
              throwing _CouldNotReleaseKio $ showErr "Failed to release" n
        let cls = liftIO $ closeFd h
        finally rel cls

  bracket init cleanup $ \env -> f (\e -> runRIO env $ sendEvent e)
