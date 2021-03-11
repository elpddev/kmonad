{-|

Module      : KMonad.Core
Description : Basic data-types of keyboards, events, times
Copyright   : (c) David Janssen, 2021
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable

-}

module KMonad.Core
  (
    HasCfg(..)
  , module KMonad.Prelude
  )
where

import KMonad.Prelude

-- | Class for anything that has a configuration
class HasCfg a c | a -> c where
  cfg :: Getter a c
