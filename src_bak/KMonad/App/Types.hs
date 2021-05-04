{-# LANGUAGE RankNTypes #-}
module KMonad.App.Types

where

import KMonad.Core

--------------------------------------------------------------------------------
{- $source

A source is any action that can be repeated to generate some `a` over and over.

Go see: "KMonad.App.Signal" for:
-- 'withSource' :: Source m a

-}

class Source m a where
  next :: m a



{- $signals

Signals are guaranteed infinite lists.

Go see: "KMonad.App.Signal" for implementations of:
-- Segment - a signal that uses an error to signal termination
-- Periodic - a signal that loops forever

-}
newtype Signal a = Signal { unSignal :: [a] } deriving (Functor, Foldable)



-- -- | A source is some object that can keep producing objects of type `a` using
-- -- some functor `f a` in some monadic context `m`.
-- --
-- -- More simply put: anything that is a source can keep popping off elements
-- -- forever in some Monad.
-- class (Functor f, Applicative m) => Source f m a where
--   pop :: f a -> m (a, f a)
