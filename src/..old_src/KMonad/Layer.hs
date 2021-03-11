module KMonad.Layer

where

import KMonad.Prelude

import KMonad.Keyboard

import qualified KMonad.App.LayerStack as Ls

-- | Layers are identified by a tag that is simply a 'Text' value.
type LayerTag = Text

-- | 'LMap's are mappings from 'LayerTag'd maps from 'Keycode' to things.
type LMap a = Ls.LayerStack LayerTag Keycode a
