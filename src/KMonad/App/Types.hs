module KMonad.App.Types

where

import KMonad.App.KeyIO.Types

-- | The ADT that collects all the different possible errors
data AppErr
  = AppKioErr KioErr
