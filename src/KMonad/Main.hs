{-|

Module      : KMonad.Main
Description : The entry-point of KMonad
Copyright   : (c) David Janssen, 2021
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable

-}

module KMonad.Main
  ( main )
where

import KMonad.Prelude

import KMonad.Cmd
import KMonad.App


main :: IO ()
main = getCmd >>= \case
  (Cmd (Run ops) logCfg) -> do

    -- Temp
    let lf = (logCfg & logLvl .~ LogDebug)

    -- Initialize and start logging
    withLog lf $ \le -> do

      -- Initialize and start
      let appCfg = AppCfg le

      withApp appCfg $ flip runRIO $ do
        loop'
        loop
