{-# LANGUAGE CPP #-}
module KMonad.Keyboard.Keycode
  ( Keycode
  )
where

import KMonad.Prelude

#ifdef linux_HOST_OS
import KMonad.Keyboard.Linux.Keycode (Keycode, codeNames)
#endif

--------------------------------------------------------------------------------
-- $alias

aliases :: [(Text, [Text])]
aliases =
  [ ("esc",  ["escape"])
  , ("-",    ["minus", "min"])
  , ("=",    ["equal", "eq", "eql"])
  , ("slp",  ["zzz"])
  , ("spc",  ["space"])
  , ("pgup", ["pageup"])
  , ("pgdn", ["pagedn", "pagedown"])
  ]
-- FIXME: CONTINUE HERE

-- -- | A collection of useful aliases to refer to keycode names
-- aliases :: Q.MultiMap Keycode Text
-- aliases = Q.mkMultiMap
--   [ (KeyEnter,          ["ret", "return", "ent"])
--   , (KeyMinus,          ["min", "-"])
--   , (KeyEqual,          ["eql", "="])
--   , (KeySleep,          ["zzz"])
--   , (KeySpace,          ["spc"])
--   , (KeyPageUp,         ["pgup"])
--   , (KeyPageDown,       ["pgdn"])
--   , (KeyInsert,         ["ins"])
--   , (KeyDelete,         ["del"])
--   , (KeyVolumeUp,       ["volu"])
--   , (KeyVolumeDown,     ["voldwn", "vold"])
--   , (KeyBrightnessUp,   ["brup", "bru"])
--   , (KeyBrightnessDown, ["brdown", "brdwn", "brdn"])
--   , (KeyLeftAlt,        ["lalt", "alt"])
--   , (KeyRightAlt,       ["ralt"])
--   , (KeyCompose,        ["comp", "cmps", "cmp"])
--   , (KeyLeftShift,      ["lshift", "lshft", "lsft", "shft", "sft"])
--   , (KeyRightShift,     ["rshift", "rshft", "rsft"])
--   , (KeyLeftCtrl,       ["lctrl", "lctl", "ctl"])
--   , (KeyRightCtrl,      ["rctrl", "rctl"])
--   , (KeyLeftMeta,       ["lmeta", "lmet", "met"])
--   , (KeyRightMeta,      ["rmeta", "rmet"])
--   , (KeyBackspace,      ["bks", "bspc"])
--   , (KeyCapsLock,       ["caps"])
--   , (KeyGrave,          ["grv"])
--   , (Key102nd,          ["102d"])
--   , (KeyForward,        ["fwd"])
--   , (KeyScrollLock,     ["scrlck", "slck"])
--   , (KeyPrint,          ["prnt"])
--   , (KeyWakeUp,         ["wkup"])
--   , (KeyLeft,           ["lft"])
--   , (KeyRight,          ["rght"])
--   , (KeyLeftBrace,      ["lbrc", "["])
--   , (KeyRightBrace,     ["rbrc", "]"])
--   , (KeySemicolon,      ["scln", ";"])
--   , (KeyApostrophe,     ["apos", "'"])
--   , (KeyGrave,          ["grv", "`"])
--   , (KeyBackslash,      ["bksl", "\\"]) -- NOTE: "\\" here is a 1char string, the first \ is consumed by Haskell as an escape character
--   , (KeyComma,          ["comm", ","])
--   , (KeyDot,            ["."])
--   , (KeySlash,          ["/"])
--   , (KeyNumLock,        ["nlck"])
--   , (KeyKpSlash,        ["kp/"])
--   , (KeyKpEnter,        ["kprt"])
--   , (KeyKpPlus,         ["kp+"])
--   , (KeyKpAsterisk,     ["kp*"])
--   , (KeyKpMinus,        ["kp-"])
--   , (KeyKpDot,          ["kp."])
--   , (KeySysRq,          ["ssrq", "sys"])
--   ]
