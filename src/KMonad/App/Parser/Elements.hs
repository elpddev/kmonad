module KMonad.App.Parser.Elements

where

import KMonad.Prelude

import KMonad.Object.Name
import KMonad.App.KeyIO.Types
import KMonad.App.KeyIO.Names

keycode :: Parser Keycode
keycode = byName allKeyNames
