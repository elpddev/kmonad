{-|
Module      : KMonad.Keyboard.Windows.Types
Description : The Windows-specific representation of KeyEvent.
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

NOTE: The representation here lines up with the @keyio_win.c@ module, not with
Windows in general. There is some translation happening in the c-code.

-}
module KMonad.Keyboard.Windows.Types
  -- ( WinKeySwitch
  -- , _WinKeySwitch
  -- )

where

import KMonad.Prelude

import Foreign.Storable
import KMonad.Keyboard.Types

-- FIXME: Better error-handling

--------------------------------------------------------------------------------
-- $event
--
-- The Windows native representation of KeyEvents is as a single byte
-- identifying either a 'Press' or 'Release', and a @DWORD@ (32bit uint).

-- | The 'WindowsEvent' record
data WindowsEvent = WindowsEvent
  { _weSwitch :: Word8  -- ^ 0 for Press, 1 for Release
  , _weCode   :: Word32 -- ^ Windows indentifier for 'Keycode'
  } deriving (Eq, Ord, Show)
makeClassy ''WindowsEvent

-- | A 'Storable' instance that lets us convert our Haskell data into c-struct
-- binary sequences.
instance Storable WindowsEvent where

  -- lowest common denominator of: 1 4
  alignment _ = 4

  -- (1 + 3-padding) + 4
  sizeOf    _ = 8

  -- peek of 1 byte, then 4
  peek ptr = do
    s <- peekByteOff ptr 0
    c <- peekByteOff ptr 4
    pure $ WindowsEvent s c

  -- poke the switch into 1 byte, then the keycode into 4
  poke ptr a = do
    pokeByteOff ptr 0 $ a^.weSwitch
    pokeByteOff ptr 4 $ a^.weCode


--------------------------------------------------------------------------------
-- $conv
--
-- The following section deals with converting between 'WindowsEvent' and
-- internal 'KeyEvent's. The only real wrinkle is the fact that 'WindowsEvent's
-- don't come with a time-stamp, so converting into 'KeyEvent' requires some
-- context (like Reader or IO).

-- | Translate a 'WindowsEvent' to a KMonad 'KeyEvent'
--
-- This would fail in the theoretically possible situation where 'weSwitch'
-- doesn't equal 0 or 1. Not sure that ever happens, but the "KMonad events are
-- a subset of the OS events"-semantic lines up with the Linux design as well.

fromWindowsEvent :: WindowsEvent -> Maybe (UTCTime -> KeyEvent)
fromWindowsEvent e =
  let mk s t = mkKeyEvent s (fromIntegral $ e^.weCode) t
  in mk <$> [Press, Release] ^? ix (fromIntegral $ e^.weSwitch)

-- | Translate a 'KeyEvent' to a 'WindowsEvent'
toWindowsEvent :: KeyEvent -> WindowsEvent
toWindowsEvent e = WindowsEvent
  { _weSwitch = if e^.switch == Press then 0 else 1
  , _weCode   = fromIntegral $ e^.keycode }



-- | We can reference the switch stored inside WinKeySwitch
-- instance HasSwitch WindowsEvent where
--   switch = wSwitch . (iso (\w -> if w == 0 then Press else Release)
--                           (\s -> if s == Press then 0 else 1))
-- -- mkWinKeyEvent :: WinSwitch -> WinKeycode -> WinKeyEvent
-- -- mkWinKeyEvent s e = WinKeyEvent (s, e)

-- -- | Convert a 'KeyEvent' to a 'WinKeyEvent'
-- --
-- -- NOTE: Windows keycodes are different, and I am not confident I have full
-- -- coverage, therefore this conversion is not total. We are going to leave this
-- -- error-handling in until we are sure this is covered well. Once it lines up
-- -- perfectly, this is essentially an Iso.
-- -- toWinKey :: KeyEvent -> WinKeySwitch
-- -- toWinKeyEvent e = WinKeyEvent (e^.switch.from _WinSwitch, e^.keycode)
--   -- case toWinKeycode $ e^.keycode of
--   -- Just c  -> Right $ WinKeyEvent (e^.switch.from _WinSwitch, c)
--   -- Nothing -> Left . NoWinKeycodeTo $ e^.keycode
