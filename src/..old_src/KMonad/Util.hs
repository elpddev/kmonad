{-|
Module      : KMonad.Util
Description : Various bits and bobs that I don't know where to put
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

Contains code for making it slighly easier to work with time, errors, and
Acquire datatypes.

-}
module KMonad.Util
  ( -- * Time units and utils
    -- $time
    Milliseconds
  , HasTime(..)
  , tDiff
  , now
  , systemTime
  , waitMS

    -- * Random utility helpers that have no better home
  , onErr
  , using
  , logRethrow
  , manyToOne
  , _keys

    -- * Some helpers to launch background process
    -- $launch
  , withLaunch
  , withLaunch_
  , launch
  , launch_
  )

where

import KMonad.Prelude

import qualified RIO.HashMap as M

--------------------------------------------------------------------------------
-- $time

-- | We use 'Milliseconds' as a /diffTime/ in KMonad
newtype Milliseconds = Milliseconds Integer
  deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read, Generic, Display)

-- | A class describing how to access some value's 'UTCTime'.
class HasTime a where
  time :: Lens' a UTCTime

-- | 'UTCTime' is a member of 'HasTime'
instance HasTime UTCTime where time = id

-- | The time elapsed between objects that have a time value.
--
-- If `a` is *newer* than `b` the difference is positive and vice versa.
tDiff :: (HasTime a, HasTime b) => a -> b -> Milliseconds
tDiff a b = round . (* 1000) $ (a^.time) `diffUTCTime` (b^.time)

-- | Turn a function that takes a 'UTCTime' into a 'MonadIO' action
now :: MonadIO m => (UTCTime -> a) -> m a
now f = f <$> getCurrentTime

-- | An 'Iso' between 'UTCTime' and 'SystemTime'
systemTime :: Iso' UTCTime SystemTime
systemTime = iso utcToSystemTime systemToUTCTime

-- | Pause processing for @n@ `Milliseconds`
waitMS :: MonadIO m => Milliseconds -> m ()
waitMS = threadDelay . (1000*) . fromIntegral


--------------------------------------------------------------------------------
-- $util

-- | A helper function that helps to throw errors when a return code is -1.
-- Easiest when used as infix like this:
--
-- > someFFIcall `onErr` MyCallFailedError someData
--
onErr :: (MonadUnliftIO m, Exception e) => m Int -> e -> m ()
onErr a err = a >>= \ret -> when (ret == -1) $ throwIO err

-- | Embed the action of using an 'Acquire' in a continuation monad
using :: Acquire a -> ContT r (RIO e) a
using dat = ContT $ (\next -> with dat $ \a -> next a)

-- | Turn a (x, [a, b, c]) tuple into an [(a, x), (b, x), (c, x)] foldable
--
-- This is used primarily to work with alias-lists of the form:
-- >>> [ ("foo", ["f", "f00", "foo!1"])
-- >>> , ("bar", ["b4r", "baarrgghh"]) ]
--
-- NOTE: In the above example, the list must be viewed like
-- >>> xs ^.. folded . manyToOne
--
manyToOne :: Foldable f => Fold (a, f b) (b, a)
manyToOne = folding $ \(a, bs) -> map (,a) (toList bs)

-- | A lensy fold over 'HashMap's keys
_keys :: Fold (M.HashMap k v) k
_keys = folding $ M.keys

-- | Log an error message and then rethrow the error
--
-- Particularly useful as a suffix using `catch`. i.e.
--
-- > doSomething `catch` logRethrow "I caught something"
logRethrow :: HasLogFunc e
  => Text
  -> SomeException -- ^ The error to throw
  -> RIO e a
logRethrow t e = do
  logError $ display t <> ": " <> display e
  throwIO e


--------------------------------------------------------------------------------
-- $launch
--
-- We have a few utility functions that launch monadic actions as async
-- processes and automatically perform logging and error-handling.

-- | Launch a process that repeats an action indefinitely. If an error ever
-- occurs, print it and rethrow it. Ensure the process is cleaned up upon error
-- and/or shutdown.
withLaunch :: HasLogFunc e
  => Text                   -- ^ The name of this process (for logging)
  -> RIO e a                -- ^ The action to repeat forever
  -> ((Async a) -> RIO e b) -- ^ The foreground action to run
  -> RIO e b                -- ^ The resulting action
withLaunch n a f = do
  logDebug $ "Launching process: " <> display n
  withAsync
   (forever a
    `catch`   logRethrow ("Encountered error in <" <> textDisplay n <> ">")
    `finally` logDebug   ("Closing process: " <> display n))
   (\a' -> link a' >> f a')

-- | Like withLaunch, but without ever needing access to the async process
withLaunch_ :: HasLogFunc e
  => Text    -- ^ The name of this process (for logging)
  -> RIO e a -- ^ The action to repeat forever
  -> RIO e b -- ^ The foreground action to run
  -> RIO e b -- ^ The resulting action
withLaunch_ n a f = withLaunch n a (const f)

-- | Like 'withLaunch', but in the ContT monad
launch :: HasLogFunc e
  => Text    -- ^ The name of this process (for logging)
  -> RIO e a -- ^ The action to repeat forever
  -> ContT r (RIO e) (Async a)
launch n = ContT . withLaunch n

-- | Like 'withLaunch_', but in the ContT monad
launch_ :: HasLogFunc e
  => Text    -- ^ The name of this process (for logging)
  -> RIO e a -- ^ The action to repeat forever
  -> ContT r (RIO e) ()
launch_ n a = ContT $ \next -> withLaunch_ n a (next ())
