module KMonad.Object.Stream.Types

where

import KMonad.Prelude

--------------------------------------------------------------------------------
-- $basic

data Stream a = Cons a (Stream a)

instance Functor Stream where
  fmap f (Cons a as) = Cons (f a) $ fmap f as

instance Applicative Stream where
  pure a = Cons a $ pure a
  (Cons f fs) <*> (Cons a as) = Cons (f a) (fs <*> as)

instance Comonad Stream where
  extract (Cons a _) = a
  duplicate (Cons a as) = Cons (Cons a as) (duplicate as)

instance Foldable Stream where
  foldMap f (Cons a as) = f a <> foldMap f as

instance Show a => Show (Stream a) where
  show (Cons a as) = show a <> ": " <> show as

data StreamException = EndOfStreamException deriving Show
makeClassyPrisms ''StreamException

instance Exception StreamException
instance AsStreamException SomeException where _StreamException = exception

--------------------------------------------------------------------------------
-- $subtypes

data Race
