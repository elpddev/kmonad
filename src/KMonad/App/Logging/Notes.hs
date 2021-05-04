module KMonad.App.Logging.Notes

where


import KMonad.Prelude

import KMonad.App.Logging
import KMonad.Object.Time

go :: OnlyIO ()
go = runLog def entry

entry :: LIO m env => m ()
entry = do
  section
  say  "Hello there, this is some raw text"
  print $ (800 :: Ms)
  section
