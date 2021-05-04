module KMonad.App.KeyIO.Notes

where

import KMonad.Prelude

import KMonad.App.Logging
import KMonad.App.KeyIO

go :: OnlyIO ()
go = runLog def $ do
  runTest "abcdefg"
  runTest "hijklmnop"

runTest :: String -> OnlyLIO ()
runTest s = do

  say $ "Running test: " <> pack s

  let cfg = mkKioCfg (ListGetCfg s) LogPutCfg :: KioCfg Char
  let go  = forever $ getKey >>= putKey
  withKio cfg $ \kio -> runRIO kio $ go
    `catch` \(e :: EOT) -> say "Finished test" >> section


test :: OnlyLIO ()
test = do
  let cfg = mkKioCfg (ListGetCfg "abcdef") LogPutCfg :: KioCfg Char
  withKio cfg $ \kioEnv ->
    runRIO kioEnv $ forever (getKey >>= putKey)

