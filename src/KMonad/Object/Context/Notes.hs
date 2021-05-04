module KMonad.Object.Context.Notes

where

import KMonad.Prelude
import KMonad.Prelude.Example

import KMonad.Object.Context.Types
import KMonad.Object.Context.Operations

-- | Create a Ctx of `running on some int`
onN :: Int -> Ctx r OnlyIO Int
onN i = mkCtx $ \f -> do
  print $ "Acquiring " <> show i
  res <- f i
  print $ "Releasing " <> show i
  pure res

-- | Nesting illustration
beholdNesting :: OnlyIO ()
beholdNesting = runCtx (nest (map onN [1..10])) print
