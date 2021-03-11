{-|
Module      : Main
Description : The entrypoint for the KMonad application
Copyright   : (c) David Janssen, 2021
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable

note: we just import main from "KMonad.Main". The reason for this lib/app
seperation is that @haddock@ will only generate documentation for libraries, so
all of KMonad is built as a library.


-}
module Main
  ( -- * The entry-point to KMonad
    main
  )
where

import KMonad.Main ( main )
