module KMonad.App.OS.Test

where

import KMonad.Prelude

import KMonad.App.KeyIO
import KMonad.App.OS.Types


-- | Initialize a ListGet input by parsing text
--
-- NOTE: We use the 'Parse' instance from 'CanKeycode' to read a series of
-- keycodes from text.
testLIT :: CanKeycode e => GetToken -> ListGetCfg e
testLIT (ListGet s) = ListGetCfg $ parseThrow (some parser) s

testLOT :: PutToken -> LogPutCfg
testLOT LogPut = LogPutCfg
