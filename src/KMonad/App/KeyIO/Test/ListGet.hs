module KMonad.App.KeyIO.Test.ListGet

where

import KMonad.Prelude
import KMonad.Object.Context
import KMonad.App.Logging
import KMonad.App.KeyIO.Types

-- | Exception that is thrown when the test-input is completely consumed
data EOT = EOT deriving Show
instance Exception EOT

data ListGetCfg = ListGetCfg
  { _input :: [Keycode] }
makeClassy ''ListGetCfg

data ListGetEnv = ListGetEnv
  { _remaining :: IORef [Keycode]
  , _lgLogEnv  :: LogEnv
  }
makeClassy ''ListGetEnv

instance HasLogEnv ListGetEnv where logEnv = lgLogEnv

-- | Pop the next element from the env, throw an EOT exception if done
pop :: RIO ListGetEnv Keycode
pop = do
  ref <- view remaining
  uncons <$> readIORef ref >>= \case
    Nothing      -> throwM EOT
    Just (a, as) -> writeIORef ref as >> pure a


withListGet :: LUIO m env => ListGetCfg -> Ctx r m GetKey
withListGet c = mkCtx $ \f -> do

  let init = do
        h  <- newIORef $ c^.input
        le <- view logEnv
        pure $ ListGetEnv h le

  let cleanup _ = do
        section

  let go env = f (runRIO env pop)

  bracket init cleanup go
