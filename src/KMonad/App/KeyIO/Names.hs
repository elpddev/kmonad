module KMonad.App.KeyIO.Names

where

{-
NOTE: We break the usual rules about importing here, and form a cycle:

App.Parser needs these dictionaries to specify a keycode parser
App.KeyIO needs the keycode parser to specify its ListGet test input

It works out fine because the submodules don't rely on eachother, but still: a
slight sloppiness. However, the solutions to this problem seem worse than the
problem.
-}


import KMonad.Prelude
import KMonad.Object.Name
import KMonad.App.KeyIO.Types

import qualified RIO.HashMap as M

-- | Reversible map from Text to Maybe Keycode
_KeyName :: Prism' Text Keycode
_KeyName = prism' toName fromName where
  toName c = case M.lookup c revNames of
    Just t -> t
    Nothing -> "<" <> tshow c <> ">"
  -- We look up a name by checking a number of places in sequence
  fromName = flip M.lookup allKeyNames

-- | A map of standard aliases that should work on any os
--
-- NOTE: This means that any name mapped to should be supported by the keyNames
-- for that OS.
stdAliases :: [Alias]
stdAliases =
  -- manyMap maps all the alternative to the _1 value of the tuple
  [ ("ret" , ["return", "ent"])
  , ("-"   , ["min", "minus"])
  , ("="   , ["eql", "equal"])
  , ("slp" , ["zzz", "sleep"])
  , ("spc" , ["space"])
  , ("pgup", ["pageup"])
  , ("pgdn", ["pagedown"])
  , ("ins" , ["insert"])
  , ("del" , ["delete"])
  , ("volu", ["volup", "volumeup", "vol+"])
  , ("vold", ["voldwn", "vol-", "voldown"])
  , ("brup", ["bru", "br+"])
  , ("brdn", ["brdown", "brdwn", "br-"])
  , ("lalt", ["alt"])
  , ("lctl", ["ctl", "ctrl", "lctrl", "control"])
  , ("lsft", ["sft", "shft", "lshift"])
  , ("`"   , ["grv"])
  ]

allKeyNames :: NameMap Keycode
allKeyNames = insertAliases stdAliases $ insertAliases keyAliases $ keyNames

revNames :: M.HashMap Keycode Name
revNames = reverseMap keyNames
