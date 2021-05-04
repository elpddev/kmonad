{-|
Module      : KMonad.Keyboard.Windows.IO.SendEventSink
Description : Using Windows' send_event functionality to inject KeyEvent's
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

This uses @sendKey@ from the @keyio_win.c@ to send keys to Windows. This itself
then uses the Windows 'SendInput' system call.

-}
module KMonad.Keyboard.Windows.IO.SendEventSink
  ( sendEventKeySink
  )
where

import KMonad.Prelude

import Foreign.Ptr
import Foreign.Marshal hiding (void)
import Foreign.Storable

import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Keyboard.Windows.Types
import KMonad.Util

import qualified RIO.HashMap as M

--------------------------------------------------------------------------------
-- $ffi
--
-- The C-function wrappings for this module

-- | The C-function that sends an event to Windows through @SendKey@
foreign import ccall "sendKey"
  c_sendKey :: Ptr WindowsEvent -> IO ()

--------------------------------------------------------------------------------
-- $rep
--
-- In Windows the keystream is encoded as a series of press events. Every press
-- event triggers a key-repeat. KMonad functions only on alternating
-- Press/Release events, so we have to manually add the key-repeat behavior back
-- in.

-- | The 'Repeater' record
data Repeater a = Repeater
  { _delay :: Milliseconds
  , _rate  :: Milliseconds
  , _procs :: MVar (M.HashMap a (Async ()))
  }
makeClassy ''Repeater

-- | Initialize the 'KeyRepeat' environment
mkRepeater :: Hashable a
  => Milliseconds       -- ^ Delay until we start repeating
  -> Milliseconds       -- ^ Time between repeats
  -> RIO e (Repeater a) -- ^ The environment
mkRepeater d r = Repeater d r <$> newMVar M.empty

-- | Create an IO action that waits the appropriate time and then repeats forever
mkProc :: HasRepeater e a => IO b -> RIO e b
mkProc io = do
  waitMS =<< view delay
  forever $ liftIO io >> (waitMS =<< view rate)
 
-- | Start repeating an action and store its identifier
repStart :: (HasRepeater e a, Hashable a, Eq a)
  => a        -- ^ The index to store this action under
  -> IO ()    -- ^ The action to repeat in the background
  -> RIO e () -- ^ The resulting action
repStart c io = do
  a <- async $ (void $ mkProc io)
  m <- view procs
  modifyMVar_ m $ \ps -> pure $ M.insert c a ps

-- | Stop the action
repStop :: (HasRepeater e a, Hashable a, Eq a) => a -> RIO e ()
repStop c = do
  m <- view procs
  modifyMVar_ m $ \ps -> do
    case M.lookup c ps of
      Nothing -> pure ps
      Just a -> do
        cancel a
        pure $ M.delete c ps

--------------------------------------------------------------------------------

-- | The environment we need to perform the sink operations
data SendEnv = SendEnv
  { _ptr  :: Ptr WindowsEvent -- ^ Pointer used to communicate with C
  , _rep  :: Repeater Keycode -- ^ Repeater environment
  , _logF :: LogFunc          -- ^ Log-function
  }
makeClassy ''SendEnv

instance HasLogFunc  SendEnv         where logFuncL = logF
instance HasRepeater SendEnv Keycode where repeater = rep

-- | Type alias for IO inside 'SendEnv'
type S a = RIO SendEnv a

-- | Return a 'KeySink' using Window's @sendEvent@ functionality.
sendEventKeySink :: HasLogFunc e
  => Milliseconds -- ^ Delay before a key starts repeating
  -> Milliseconds -- ^ Time between key-repeats
  -> RIO e (Acquire KeySink)
sendEventKeySink d r =
  let open   = skOpen d r
      clse v = runRIO v skClose
      send v = runRIO v . skSend
  in mkKeySink open clse send

-- | Create the 'Wenv' environment
skOpen :: HasLogFunc e => Milliseconds -> Milliseconds -> RIO e SendEnv
skOpen d r = do
  logInfo "Initializing Windows key sink"
  pt <- liftIO $ mallocBytes (sizeOf (undefined :: WindowsEvent))
  rp <- mkRepeater d r
  lf  <- view logFuncL
  pure $ SendEnv pt rp lf

-- | Close the 'Wenv' environment
skClose :: S ()
skClose = do
  logInfo "Closing Windows key sink"
  liftIO . free =<< view ptr

-- | Send a 'WindowsEvent' to Windows
sendKey :: WindowsEvent -> S ()
sendKey e = view ptr >>= \buf -> liftIO (poke buf e >> c_sendKey buf)

-- | Write an event to the pointer and prompt windows to inject it
skSend :: KeyEvent -> S ()
skSend e = do
  let e' = toWindowsEvent e
  u <- askRunInIO
  sendKey e'
  if isPress e
    then repStart (e^.keycode) (u $ sendKey e')
    else repStop  (e^.keycode)
