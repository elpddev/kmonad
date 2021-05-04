module KMonad.Logic.Stream

where

import KMonad.Prelude

import Control.Comonad
import Control.Exception (throw)

import KMonad.Core
import qualified RIO.List  as L (cycle, take, iterate, repeat)

data StreamError = EndOfList

instance Exception StreamError
instance Show StreamError where
  show EndOfList = "Encountered end of list in stream"


data Stream a = Cons a (Stream a)
  deriving Show

instance Functor Stream where
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Comonad Stream where
  extract (Cons a _) = a
  duplicate (Cons a as) = Cons (Cons a as) (duplicate as)

instance Foldable Stream where
  foldMap f (Cons a as) = f a <> foldMap f as

take :: Int -> Stream a -> [a]
take n = L.take n . toList

-- | Turn a list into a stream, this will throw an error if the list is ever
-- exhausted. Don't use it on non-infinite lists.
unsafeFromList :: [a] -> Stream a
unsafeFromList [] = throw EndOfList
unsafeFromList (a:as) = Cons a (unsafeFromList as)

-- | Turn a finite but non-empty list into a repeating stream
periodic :: NonEmpty a -> Stream a
periodic (a:|as) = unsafeFromList $ L.cycle (a:as)

-- | Stream of all natural numbers, useful for playing
nats :: Stream Int
nats = unsafeFromList $ L.iterate (+1) 1

repeat :: a -> Stream a
repeat = unsafeFromList . L.repeat

instance IsString (Stream Char) where
  fromString []     = repeat '?'
  fromString (a:as) = periodic $ a :| as


-- | Run an action that requires a list by repeatedly using an action to
-- generate elements. The action that generates elements can setup some context
-- itself.
withStreamIO :: (MonadUnliftIO m)
  => m a
  -> (Stream a -> m b)
  -> m b
withStreamIO gen f = do
  c <- newChan
  withAsync (forever $ gen >>= writeChan c) $ const $
    f . unsafeFromList =<< getChanContents c

-- | Run a stream until an EndOfStream error is caught, return the result
--
-- Mainly useful when testing stuff on finite examples
runStream :: UIO m => Stream a -> m [a]
runStream s = do
  h <- newIORef []
  flip finally (pure ()) $ forM_ (toList s) (modifyIORef h . (:))
  readIORef h
