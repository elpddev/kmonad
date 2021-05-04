{-# LANGUAGE RankNTypes #-}
module KMonad.Context
  ()
where

import KMonad.Prelude

import qualified RIO.Text.Lazy as L

-- | A 'Ctx' in a situation in which some `a` can be calculated, such that any
-- functions that takes an `a` and then does something, can be made to do so,
-- and then we return the result.
type Ctx a m b = (a -> m b) -> m b

onIO :: MonadUnliftIO m => FilePath -> Ctx L.Text m a
onIO = withLazyFileUtf8

-- Cute, but it doesn't make the code clearer, I prefer (a -> m b) -> m b
-- However, in the documentation, `Ctx` should refer tho this type.
