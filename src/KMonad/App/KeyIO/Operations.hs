{-# LANGUAGE CPP #-}
module KMonad.App.KeyIO.Operations

where

import KMonad.Prelude
import KMonad.Object.Context
import KMonad.Object.Name
import KMonad.Object.Stream
import KMonad.Object.Time
import KMonad.App.Logging
import KMonad.App.KeyIO.Types
import KMonad.App.KeyIO.Test

import qualified RIO.HashMap as M

#ifdef linux_HOST_OS
import KMonad.App.KeyIO.Linux
#endif

#ifdef mingw32_HOST_OS
import KMonad.App.KeyIO.Windows
#endif

#ifdef darwin_HOST_OS
import KMonad.App.KeyIO.Mac
#endif

--------------------------------------------------------------------------------
-- $with

withGetKey :: LUIO m env => GetCfg -> Ctx r m GetKey -- (GetKey -> m a) -> m a
withGetKey = either withGetKeyTest withGetKeyIO

withPutKey :: LUIO m env => PutCfg -> Ctx r m PutKey -- (PutKey -> m a) -> m a
withPutKey = either withPutKeyTest withPutKeyIO

withGetters :: LUIO m env => NameMap GetCfg -> Ctx r m (Chan KeyEvent)
withGetters gs = do
  -- Start threads for every getter that writes to the KeyIO chan
  chan <- lift newChan
  let go k v = do
        getter <- withGetKey v
        launch $ writeChan chan =<< do
          t <- getCurrentTime
          c <- liftIO getter
          pure $ KeyEvent k c t
  void $ M.traverseWithKey go gs
  pure chan

withPutters :: LUIO m env => NameMap PutCfg -> Ctx r m [PutKey]
withPutters = mapM withPutKey . M.elems

-- | Open the context of all KeyIO
withKio :: (HasKioCfg cfg, LUIO m env) => cfg -> Ctx r m KioEnv
withKio c = do
  le   <- lift $ view logEnv
  chan <- withGetters $ c^.getKeyCfg
  ps   <- withPutters $ c^.putKeyCfg
  pure $ KioEnv (c^.kioCfg) le chan ps

--------------------------------------------------------------------------------
-- $actions

-- | Use the KioEnv to await the next key
getKey :: KIO m env => m KeyEvent
getKey =  readChan =<< view getChan

-- | Use the KioEnv to send a key to the OS
putKey :: KIO m env => Keycode -> m ()
putKey e = mapM_ (liftIO . ($ e)) =<< view putters

-- | Use the KioEnv to run a pure computation over a stream of inputs
-- withStream :: KUIO m env => (Stream KeyEvent -> m a) -> m a
-- withStream = withStreamIO getKey
