module KMonad.Keyboard.Windows.Keycode
  ( Keycode
  , _RawName
  , osKeynames
  , osAliases
  )
where

import KMonad.Prelude


import RIO.Text (unpack)

import KMonad.Keyboard.Keycode

--------------------------------------------------------------------------------
-- $code
--
-- We represent 'Keycode's in Windows simply as a newtype wrapper around
-- 'Word32', which is already the Windows-native representation of keycodes,
-- saving us from having to do any casting.

