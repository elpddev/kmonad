{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module KMonad.App.Signal

where

import KMonad.Prelude
import KMonad.Prelude.Example

import Control.Comonad
import Control.Exception (throw)

import KMonad.Core
import KMonad.App.Types

data StreamError = EndOfList

instance Show StreamError where
  show EndOfList = "Encountered end of list in stream"

instance Exception StreamError

data Stream a = Cons a (Stream a)
  deriving Show

instance Functor Stream where
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Comonad Stream where
  extract (Cons a _) = a
  duplicate (Cons a as) = Cons (Cons a as) (duplicate as)

instance Foldable Stream where
  foldMap f (Cons a as) = f a <> foldMap f as

-- | Turn a list into a stream, this will throw an error if the list is ever
-- exhausted. Don't use it on non-infinite lists.
unsafeFromList :: [a] -> Stream a
unsafeFromList [] = throw EndOfList
unsafeFromList (a:as) = Cons a (unsafeFromList as)

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



runStream :: UIO m => Stream a -> m [a]
runStream s = do
  h <- newIORef []
  flip finally (pure ()) $ forM_ (toList s) (modifyIORef h . (:))
  readIORef h

-- | The type of a handler that does something on an `a`, and then produces the
-- next function required.
newtype K m a = K { runQ :: a -> m (K m a) }
