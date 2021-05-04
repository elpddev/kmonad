module KMonad.Object.Time.Operations

where

import KMonad.Prelude
import KMonad.Object.Time.Types

import qualified RIO.Time as T




--------------------------------------------------------------------------------
-- $manipulations

-- | Delay a time by some milliseconds
delay :: Ms -> Time -> Time
delay (Ms ms) (Time t) = Time $ T.addUTCTime d t
  where d = fromIntegral ms / 1000

-- | Return whichever event of 2 occured first
--
-- This will require both objects to be resolved.
earliest :: (HasTime a, HasTime b) => a -> b -> Either a b
earliest a b | a^.time < b^.time = Left  a
          | otherwise         = Right b

-- | Try to do something within some time-limit, fail otherwise
within :: UIO m => Ms -> m a -> m (Maybe a)
within (Ms d) go = race (threadDelay $ d * 1000) go
  >>= pure . either (const Nothing) Just
