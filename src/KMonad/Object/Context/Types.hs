module KMonad.Object.Context.Types
  ( Ctx, mkCtx, runCtx )
where

import KMonad.Prelude
import Control.Monad.Cont

{- Context notes:

We often use this type of calling signature:
  withAbc :: AbcCfg -> (AbcEnv -> m a) -> m a

Use some config to establish the context of AbcEnv existing, then run a function
in that context, and return the result. An example of this is the
bracket-pattern, where we acquire a resource, and when we finish, we want to
ensure we clean up that resource.

Nearly all of the code in App uses these types of constructs.

Furthermore, in KeyIO we want to nest a context. Simplified, we want to be able
to treat a "[(GetKey -> m a) -> m a]" as a "([GetKey] -> m a) -> m a"

This is essentially just a Continuation monad, but without using any of the
complicated callCC mechanisms. I often find trying to comprehend documentation
harder than experimenting, so here is a simplified Cont, attempting to ease
context usage.

This module is arguably redundant and I might be better off just using ContT,
but at least this way I can centralize my notes while I experiment.

-}

newtype Ctx r m a = Ctx { unCtx :: ContT r m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

mkCtx :: ((a -> m r) -> m r) -> Ctx r m a
mkCtx = Ctx . ContT

runCtx :: Ctx r m a -> (a -> m r) -> m r
runCtx = runContT . unCtx
