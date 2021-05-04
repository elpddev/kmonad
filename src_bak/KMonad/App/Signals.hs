{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module KMonad.App.Signal

where

import KMonad.Prelude

import Control.Monad.Except
import Control.Exception (throw)

import RIO.List (cycle, repeat)
import RIO.NonEmpty (nonEmpty)
import RIO.Partial (fromJust)

data SignalError
  = SegmentEnd
  | SignalEmpty
instance Exception SignalError

instance Show SignalError where
  show SegmentEnd  = "Reached end of segment"
  show SignalEmpty = "Should be impossible, file a Github issue please"

class (Functor f, Applicative m) => Source f m a where
  pop :: f a -> m (a, f a)

-- | Signals are infinite (lazy) lists
--
-- We don't export ways of creating signals directly from lists, only ways that
-- are guaranteed to be infinite, so whenever we have a signal, we *know* we can
-- generate a future value.

newtype Signal a = Signal { unSignal :: [a] } deriving (Functor, Foldable)

instance Applicative m => Source Signal m a where
  -- Note: we don't add MonadThrow here, because we *never* expect SignalEmpty
  -- to be reachable.
  pop (Signal [])     = throw SignalEmpty
  pop (Signal (a:as)) = pure (a, Signal as)



--------------------------------------------------------------------------------
-- $periodic
--
-- A signal that keeps repeating a certain sequence, and 'remembers its place'.
-- I think this is probably just akin to a zipper, but sometimes thinking out
-- loud is better than reading documentation.

-- | Periodic of signal of type a
newtype Periodic a = Periodic { unPeriodic :: (NonEmpty a, [a])}

-- | Create a periodic signal, ensures we can only ever create non-empty versions.
mkPeriodic :: Foldable t => a -> t a -> Periodic a
mkPeriodic a = Periodic . (,[]) . (a:|) . toList

-- | Functors works as you'd expect
instance Functor Periodic where
  fmap f (Periodic (as, bs)) = Periodic (f <$> as, f <$> bs)

-- | We can peek and pop
instance Applicative m => Source Periodic m a where
  pop  (Periodic (a:|[], pst)) =
    -- fromJust is justified because (a:pst) by definition can't be empty
    let fut = fromJust $ nonEmpty $ reverse $ a:pst
    in  pure (a, Periodic (fut, []))
    -- fromJust is justified because we already matched the empty-case
  pop  (Periodic (a:|fut, pst)) =
    pure (a, Periodic (fromJust $ nonEmpty fut, a:pst))

-- | Folding over a Periodic is an infinite fold
instance Foldable Periodic where
  foldMap f (Periodic (fut, pst)) = foldMap f $ cycle $ toList fut <> reverse pst


--------------------------------------------------------------------------------
-- $segment
--
-- A segment is an infinite signal that models finality through the use of
-- throwing an error when done. As long as the signal is defined, a 'Segment'
-- produces values, the moment it is over, it throws a 'SegmentEnd' error.


newtype Segment a = Segment  { unSegment  :: [a]  }
  deriving (Functor)

-- | Create a new Segment, unlike other signals, segments can have 0 length.
mkSegment :: Foldable t => t a -> Segment a
mkSegment = Segment . toList

instance (MonadError SignalError m, Monad m) => Source Segment m a where
  pop (Segment [])      = throwError SegmentEnd
  pop (Segment (a:fut)) = pure $ (a, Segment fut)

-- | Folding over a segment will only fold over the actual signal
instance Foldable Segment where
  foldMap f (Segment fut) = foldMap f fut

--------------------------------------------------------------------------------
-- $io

withSignalIO :: MonadUnliftIO m => m a -> (Signal a -> m b) -> m b
withSignalIO go f = do
  c <- newChan
  withAsync (forever $ go >>= writeChan c) $ \a ->
    f . Signal =<< getChanContents c

mkCounter :: IO (IO Int)
mkCounter = do
  h <- newIORef (-1)
  pure $ do
    modifyIORef h (+1)
    i <- readIORef h
    threadDelay 1000000
    pure i
