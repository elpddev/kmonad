{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-|
Module      : KMonad.Prelude
Description : Code that will be imported into every module
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)
-}

module KMonad.Prelude
  ( module X
  , HasTime(..)
  , now
  , systemTime
  , fi
  , onErr
  , onJust
  )
where

import Control.Lens                as X
import Control.Monad.Cont          as X
import Data.Acquire                as X
import Data.Functor.Contravariant  as X
import Data.Time.Clock.System      as X
import GHC.Conc                    as X (orElse)
import RIO.Text                    as X (unlines, lines)
import RIO.Time                    as X

import RIO as X hiding
  (-- Not the lens stuff, I want more support for lenses from "Control.Lens"
    view, ASetter, ASetter', Lens, Getting, Lens'
  , SimpleGetter, lens, over, set, sets, to, (^.)

    -- The following line is required for newer stack releases.
    -- This is also the reason for the OPTIONS_GHC pragma
  , (^..), (^?), preview, (%~), (.~)

    -- Some stuff I'd rather default to Text
  , unlines, lines

    -- Will import these when I need it
  , some, many
  )


-- | Class for all data that contains a time value
class HasTime e where
  time :: Lens' e UTCTime

-- | Fill in a time argument with the current time
now :: MonadIO m => (UTCTime -> a) -> m a
now = flip fmap getCurrentTime

-- | An 'Iso' between 'UTCTime' and 'SystemTime'
systemTime :: Iso' UTCTime SystemTime
systemTime = iso utcToSystemTime systemToUTCTime

-- | Used so often so we provide a shorthand
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- | A helper function that helps to throw errors when a return code is -1.
-- Easiest when used as infix like this:
--
-- > someFFIcall `onErr` MyCallFailedError someData
--
-- onErr :: (MonadUnliftIO m, Exception e) => m Int -> e -> m ()
onErr :: (MonadIO m, Exception e) => m Int -> e -> m ()
onErr a err = a >>= \ret -> when (ret == -1) $ throwIO err

onJust :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
onJust _ Nothing  = pure Nothing
onJust f (Just a) = Just <$> f a
