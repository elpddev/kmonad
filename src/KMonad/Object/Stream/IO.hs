module KMonad.Object.Stream.IO

where

import KMonad.Prelude

import KMonad.Object.Context
import KMonad.Object.Stream.Types
import KMonad.Object.Stream.Operations

-- | Run an action that requires a list by repeatedly using an action to
-- generate elements. The action that generates elements can setup some context
-- itself.
withStreamIO :: (MonadUnliftIO m) => m a -> Ctx r m (Stream a)
withStreamIO gen = mkCtx $ \f -> do
  c <- newChan
  withAsync (forever $ gen >>= writeChan c) $ const $
    f . unsafeFromInf =<< getChanContents c
