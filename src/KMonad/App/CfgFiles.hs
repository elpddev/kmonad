module KMonad.App.CfgFiles

where

import KMonad.Prelude

import KMonad.App.KeyIO
import KMonad.App.Main.Types


loadCfg :: IO m => FilePath -> m RunCfg
loadCfg _ = pure $ def
  -- { _itoken = ListGet "hello david"
  -- , _otoken = LogPut }
