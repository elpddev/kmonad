{-|
Module      : KMonad.Keyboard.Linux.Keycode
Description : How Linux identifies its keys
Copyright   : (c) David Janssen, 2020
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

NOTE: we'd like to include this directly in the "KMonad.Keyboard.Linux.Types"
module, but that module needs to import "KMonad.Keyboard.Types", which in turn
needs to import the OS-specific 'Keycode' definition (i.e. this file).
Therefore, to avoid circular imports, this definition needs to exist in
isolation.
-}
module KMonad.Keyboard.Linux.Keycode
  ( Keycode
  , _RawName
  , osKeynames
  , osAliases
  )

where

import KMonad.Prelude

import RIO.Text (unpack)

import KMonad.Keyboard.Keycode as K

--------------------------------------------------------------------------------
-- $code
--
-- We represent 'Keycode's in Linux simply as a newtype wrapper around 'Word16',
-- which is already the Linux-native representation of keycodes, saving us from
-- having to do any casting.

-- | The 'Keycode' type for linux
newtype Keycode = Keycode Word16
  deriving (Eq, Ord, Enum, Num, Real, Integral, Generic, Hashable, Show)

-- | How to parse and display raw 'Keycode' values from 'Text'
_RawName :: Prism' Text Keycode
_RawName = prism' kc2txt txt2kc
  where kc2txt (Keycode c) = textDisplay c
        txt2kc t           = Keycode <$> (readMaybe . unpack) t

-- | Raw data list of all 'Keycode' 'Keyname' correspondences
osKeynames :: [(Keycode, Keyname)]
osKeynames =
  zip [1..83]
    [ "esc", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" , "-" , "="
    , "bspc", "tab", "q", "w", "e", "r", "t", "y", "u" , "i", "o" , "p", "["
    , "]", "ret", "lctl", "a", "s", "d", "f" , "g", "h", "j" , "k", "l", ";"
    , "'", "`", "lsft", "\\", "z" , "x", "c", "v", "b" , "n", "m", ",", ".", "/"
    , "rsft", "kp*" , "lalt", "spc", "caps" , "f1", "f2", "f3", "f4", "f5", "f6"
    , "f7", "f8", "f9", "f10" , "num", "scrl", "kp7", "kp8", "kp9" , "kp-"
    , "kp4", "kp5", "kp6" , "kp+", "kp1", "kp2", "kp3", "kp0" , "kp."] <>
  zip [85..120]
    [ "zenk", "102d", "f11", "f12", "ro", "kata", "hira", "henk", "kahi"
    , "muhe", "kpj,", "kprt", "rctl", "kp/", "ssrq", "ralt", "feed", "home"
    , "up", "pgup", "left", "rght", "end", "down", "pgdn", "ins", "del", "macr"
    , "mute", "vold", "volu", "pwr", "kp=", "kp+-", "paus", "scl"
    ] <>
  zip [121..182]
    [ "kp,", "hang", "hanj", "yen", "lmet", "rmet", "cmps", "stop", "agn"
    , "prps", "undo", "frnt", "copy", "open", "past", "find", "cut", "help"
    , "menu", "calc", "setp", "slp", "wake", "file", "send", "delf", "xfer"
    , "prg1", "prg2", "www", "msds", "coff", "scnl", "rot", "dir", "cycl"
    , "mail", "book", "comp", "back", "fwd", "clcd", "ejct", "eccd", "nxsg"
    , "plps", "prvs", "stcd", "rec", "rew", "phon", "iso", "cfg", "hmpg", "refr"
    , "exit", "move", "edit", "scup", "scdn", "kp(", "kp)", "new", "redo" ] <>
  zip [183..194]
    [ "f13", "f14", "f15", "f16", "f17", "f18", "f19", "f20", "f21", "f22"
    , "f23", "f24"] <>
  zip [200..248]
    [ "plcd", "pscd", "prg3", "prg4", "dash", "susp", "cls", "play", "ffwd"
    , "bass", "prnt", "hp", "cmra", "soun", "ques", "emal", "chat", "srch"
    , "conn", "fina", "sprt", "shop", "alte", "cncl", "brdn", "brup", "medi"
    , "svid", "kbtg", "kbdn", "kbup", "ksnd", "repl", "fwml", "save", "docs"
    , "batt", "blue", "wlan", "uwb", "unkn", "vdnx", "vdpv", "brcc", "brau"
    , "doff", "wwan", "wmax", "rfkl", "mcmu" ]

-- | Raw data list of various 'Keyname' correspondences for specifically linuxy
-- key-names.
osAliases :: [Alias]
osAliases =
  [ ("hira", ["hiranaga", "hirnag"])
  , ("kata", ["katakana", "katkan"])
  , ("cmps", ["cmp"])
  ]
