{-|
Module      : KMonad.Keyboard.Windows.IO.LowLevelHookSource
Description : Load and acquire a windows low-level keyboard hook.
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

-}
module KMonad.Keyboard.Windows.IO.LowLevelHookSource
  ( llHook
  )
where

import KMonad.Prelude

import Foreign.Marshal hiding (void)
import Foreign.Ptr
import Foreign.Storable

import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Keyboard.Windows.Types
import KMonad.Util (now)

--------------------------------------------------------------------------------

-- | Use the windows c-api to `grab` a keyboard
foreign import ccall "grab_kb"
  grab_kb :: IO Word8

-- | Release the keyboard hook
foreign import ccall "release_kb"
  release_kb :: IO Word8

-- | Pass a pointer to a buffer to wait_key, when it returns the buffer can be
-- read for the next key event.
foreign import ccall "wait_key"
  wait_key :: Ptr WindowsEvent -> IO ()


--------------------------------------------------------------------------------

-- | Data used to track `connection` to windows process
data LLHook = LLHook
  { _thread :: !(Async Word8)        -- ^ The thread-id of the listen-process
  , _buffer :: !(Ptr WindowsEvent) -- ^ Buffer used to communicate with process
  }
makeLenses ''LLHook

-- | Return a KeySource using the Windows low-level hook approach.
llHook :: HasLogFunc e => RIO e (Acquire KeySource)
llHook = mkKeySource llOpen llClose llRead


--------------------------------------------------------------------------------

-- | Ask windows to install a hook and allocate the reading-buffer
llOpen :: HasLogFunc e => RIO e LLHook
llOpen = do
  logInfo "Registering low-level Windows keyboard hook"
  liftIO $ do
    tid <- async grab_kb
    buf <- mallocBytes $ sizeOf (undefined :: WindowsEvent)
    pure $ LLHook tid buf

-- | Ask windows to unregister the hook and free the data-buffer
llClose :: HasLogFunc e => LLHook -> RIO e ()
llClose ll = do
  logInfo "Unregistering low-level Windows keyboard hook"
  liftIO $ do
    _ <- release_kb
    cancel $ ll^.thread -- This might not be necessary, but it is safer
    free   $ ll^.buffer

-- | Get a new 'KeyEvent' from Windows
--
-- NOTE: This can throw an error if the event fails to convert.
llRead :: HasLogFunc e => LLHook -> RIO e KeyEvent
llRead ll = do
  we <- liftIO $ do
    wait_key $ ll^.buffer
    peek $ ll^.buffer
  case fromWindowsEvent we of
    Just f  -> now f
    Nothing -> llRead ll
    -- Nothing -> error "Received unparseable Windows event"-- FIXME: better error-handling
