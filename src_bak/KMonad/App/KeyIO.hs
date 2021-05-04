module KMonad.App.KeyIO

where

import KMonad.Prelude



--------------------------------------------------------------------------------
-- $bsc

type GetKey m c = m (KeyEvent c)
type PutKey m c = KeyEvent c -> m ()

data KeyIOFunc c = KeyIOFunc
  { _getWith :: !(GetKey IO c)
  , _putWith :: !(PutKey IO c)
  }
makeClassy ''KeyIOFunc

mkKeyIOFunc :: (CanKeycode c, MonadUnliftIO m)
  => GetKey m c -> PutKey m c -> m (KeyIOFunc c)
mkKeyIOFunc g p = withRunInIO $ \u ->
  pure $ KeyIOFunc (u g) (u . p)

--------------------------------------------------------------------------------
-- $class

class CanKeyIO a where

  type Keycode  (a :: *) = c | c -> a
  type KeyIOCfg (a :: *) = f | f -> a

  withKeyIOFunc :: MonadUnliftIO m
    => KeyIOCfg a
    -> (KeyIOFunc (Keycode a) -> m b)
    -> m b

class HasKeyIO env c | env -> c where
  keyIO :: Getter env (KeyIOFunc c)

getKey :: (MonadReader env m, MonadIO m, HasKeyIO env c)
  => m (KeyEvent c)
getKey = view keyIO >>= \kio -> liftIO $ _getWith kio

putKey :: (MonadReader env m, MonadIO m, HasKeyIO env c)
  => KeyEvent c -> m ()
putKey e = view keyIO >>= \kio -> liftIO $ _putWith kio e
