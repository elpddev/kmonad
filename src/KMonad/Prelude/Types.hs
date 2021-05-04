{-# OPTIONS_GHC -Wno-orphans #-}
module KMonad.Prelude.Types

where


import RIO hiding (IO, catch)

import Control.Lens (Getter)
import Control.Monad.Catch
-- import qualified Control.Exception as C

import qualified RIO.Time    as T
import qualified RIO         as R
import qualified RIO.HashMap as M

instance MonadCatch (RIO e) where catch = R.catch
--   catch = C.catch

--------------------------------------------------------------------------------
-- $props
--
-- NOTE: Most of these are just here for overloaded fieldnames, i.e. I want to
-- use `val` for anything that has a value, and `cfg` for anything that has a
-- config, without creating many prefixed accessors (like. logCfg, appCfg,
-- ioCfg).

-- | A class that describes how to get at things that contain some decorated
-- value.
class HasVal a t where val :: Lens' a t

-- | A class that describes types that have a default value.
--
-- NOTE: default values are mainly specified to make the developer's life
-- easier, not because they are some ideal or optimal value in real life. Having
-- default values just lets me run bits of code in the REPL without having to
-- always specify some config first.
class Default a where def :: a

-- | A type that describes the oft-used continuation-running
type With cfg env m a = cfg -> (env -> m a) -> m a


--------------------------------------------------------------------------------
-- | Common IO-type constraints. Shorter words for common phrases.

-- | Shorthand for the actual IO type (not the constraint)
type OnlyIO = R.IO
-- | Shorthand for MTL-constraints of MonadIO
type IO     m     = (MonadIO       m, MonadCatch m)
-- | Shorthand for MTL-constraints of MonadUnliftIO
type UIO    m     = (MonadUnliftIO m, MonadCatch m)
-- | Shorthand for MTL-constraints of MonadIO with a Reader env
type EnvIO  m env = (MonadIO       m, MonadReader env m)
-- | Shorthand for MTL-constraints of MonadUnliftIO with a Reader env
type EnvUIO m env = (MonadUnliftIO m, MonadReader env m)

-- | Also see: LIO and LUIO in "KMonad.App.Logging"

--------------------------------------------------------------------------------
-- | Shorthand I use to sometimes reason out ideas, forgive the lyrical bent.

-- | General definition of 'Observation', but the functional shorthand of (->)
-- is nicer, but whenever I talk about an observation it is the translation of
-- some world state, through some interpretation, into some value.
type Observation a b = a -> b

-- | A predicate is an observation that ends in a boolean value.
type Predicate a = Observation a Bool
