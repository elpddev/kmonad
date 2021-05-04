{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
module KMonad.App.Button

where

import KMonad.Prelude
import Control.Applicative (ZipList(..), getZipList)
import RIO.List
import RIO.List.Partial (head)
import RIO.Partial (fromJust)

import System.IO


class Signal m a where
  type Elem (a :: *) -- = e | e -> a
  peek :: a -> m (Elem a)
  pop  :: a -> m (Elem a, a)

instance Signal Maybe [a] where
  type Elem [a] = a
  peek       = headMaybe
  pop []     = Nothing
  pop (a:as) = Just (a, as)

instance MonadIO m => Signal m (IOSignal a) where
  type Elem (IOSignal a) = a
  peek = peekIO
  pop a = (,a) <$> popIO a

newtype IOSignal a = IOSignal { unIOS :: (IO a, MVar a, MVar ()) }

mkIOSignal :: MonadIO m => IO a -> m (IOSignal a)
mkIOSignal new = IOSignal <$> pure new <*> newEmptyMVar <*> newMVar ()

peekIO :: MonadIO m => IOSignal a -> m a
peekIO (IOSignal (new, var, lock)) = do
  _ <- takeMVar lock
  tryReadMVar var >>= \case
    Just a  -> putMVar lock () >> pure a
    Nothing -> do
      a <- liftIO new
      putMVar var a
      putMVar lock ()
      pure a

popIO :: MonadIO m => IOSignal a -> m a
popIO (IOSignal (new, var, lock)) = do
  _ <- takeMVar lock
  tryTakeMVar var >>= \case
    Just a  -> putMVar lock () >> pure a
    Nothing -> liftIO new >>= \a -> putMVar lock () >> pure a



nums :: [Int]
nums = [1, 2, 3, 4]



ios :: MonadUnliftIO m => m (IOSignal Int)
ios = do
  st <- newIORef (0 :: Int)
  mkIOSignal . runRIO st  do
    h <- ask
    v <- readIORef h
    modifyIORef h (+1)
    pure v


test :: Maybe (Elem [Int])
test = peek nums

-- | 'Signal's are infinite lists
-- newtype Signal a = Signal { unSignal :: [a] }
--   deriving (Functor, Show)

-- -- | 'Segment's are finite signals modeled by Maybe, they run to first Nothing
-- --
-- -- Primarily used for tests and debugging.
-- type Segment a = Signal (Maybe a)

-- -- | Permutation is reorderings of a signal
-- type Permutation a = Signal a -> Signal a

-- foo :: Segment Char
-- foo = fromList "abcbca"

-- bar :: Segment Int
-- bar = fromList [1, 2, 3, 4]

-- runSegment :: (a -> b) -> Segment a -> [b]
-- runSegment f = map f . getTest

-- getSignal :: Signal a -> [a]
-- getSignal = getZipList . unSignal

-- getTest :: TestSignal a -> [a]
-- getTest = map fromJust . takeWhile isJust . getSignal

-- fromList :: [a] -> Segment a
-- fromList as = Signal $ ZipList $ map Just as <> repeat Nothing

-- | Because we don't want handlers messing with the input signal, we force them
-- to return 'Rest', and then don't create any ways of making 'Rest's, except by
-- taking from the input-signal.
-- newtype Rest a = Rest { unRest :: Signal a }

-- peek :: Signal a -> a
-- peek = head . getZipList . unSignal

-- pop :: Signal a -> (a, Signal a)
-- pop = case

-- class Signal l a where
--   type Elem (a :: *) = e | e -> a
--   next :: m (Elem a)

-- newtype Segment a = Segment { unSegment :: [a] }

-- instance Signal Segment a where
--   type Elem (Segme)

-- newtype Periodic a = Periodic { unPeriodic :: [a] }

-- newtype Await a = Await { unAwait :: IO a }
