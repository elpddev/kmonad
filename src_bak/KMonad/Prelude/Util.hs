module KMonad.Prelude.Util
  (metronome, Tick)
where

import RIO hiding (IO)
import RIO.Text

import KMonad.Types



newtype Tick = Tick (Int, Time)

instance Show Tick where
  show (Tick (n, t)) = show n <> ": " <> show t

-- | Do a tick every n deciseconds
metronome :: IO m => Int -> m (m Tick)
metronome d = do
  h <- newIORef 0
  pure $ do
    threadDelay (d * 10000)
    modifyIORef h (+1)
    t <- getCurrentTime
    n <- readIORef h
    pure $ Tick $ (n, t)

