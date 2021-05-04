module KMonad.Object.Context.Operations

where

import KMonad.Prelude
import KMonad.Object.Context.Types

import UnliftIO.Async (wait)

nest :: [Ctx r m a] -> Ctx r m [a]
nest = traverse id

-- | Repeat an IO action forever, in the background. Stop process when finished.
launch :: UIO m => m a -> Ctx r m ()
launch go = mkCtx $ \f -> withAsync (forever go) (const $ f ())
