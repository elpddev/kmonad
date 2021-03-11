module KMonad.Core.Time

where

import KMonad.Prelude

import Data.Time.Clock.System      as X
import RIO.Time                    as X

-- -- | Class for all data that contains a time value
-- class HasTime e where
--   time :: Lens' e UTCTime

-- -- | Fill in a time argument with the current time
-- now :: MonadIO m => (UTCTime -> a) -> m a
-- now = flip fmap getCurrentTime

-- -- | An 'Iso' between 'UTCTime' and 'SystemTime'
-- systemTime :: Iso' UTCTime SystemTime
-- systemTime = iso utcToSystemTime systemToUTCTime
