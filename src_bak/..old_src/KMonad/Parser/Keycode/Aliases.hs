module KMonad.Parser.Keycode.Aliases

where

import KMonad.Prelude

import KMonad.Keyboard

import qualified RIO.HashMap as M

-- | A map of keycode names to 'Keycode's.
--
-- This is combination of the OS-specific keycode names defined in their respective
keyNames :: M.HashMap Text Keycode
keyNames = undefined



-- aliases :: [(Text, [Text])]
-- aliases =
--   [ ("esc",  ["escape"])
--   , ("-",    ["minus", "min"])
--   , ("=",    ["equal", "eq", "eql"])
--   , ("slp",  ["zzz"])
--   , ("spc",  ["space"])
--   , ("pgup", ["pageup"])
--   , ("pgdn", ["pagedn", "pagedown"])
--   ]

-- | A name-to-name mapping defining 'Keycode' aliases.
aliases :: [Alias]
aliases =
  [ ("ret",          ["ret", "return", "ent"])
  , ("-",            ["min", "minus"])
  , ("=",          ["eql", "equal"])
  , ("slp",          ["zzz", "sleep"])
  , ("spc",          ["space"])
  , ("pgup",         ["pageup"])
  , ("pgdn",       ["pagedown"])
  , ("ins",         ["insert"])
  , ("del",         ["delete"])
  , ("volu",         ["volup", "volumeup", "vol+"])
  , ("vold",     ["voldwn", "vol-", "voldown"])
  , ("brup",   ["bru", "br+"])
  , ("brdn", ["brdown", "brdwn", "br-"])
  , ("lalt", ["alt"])
  , ("lctl", ["ctl", "ctrl", "lctrl", "control"])
  , ("lsft", ["sft", "shft", "lshift"])
  , ("`",    ["grv"])


  -- TODO: CONTINUE monkeywork HERE
  -- , (KeyRightAlt,       ["ralt"])
  -- , (KeyCompose,        ["comp", "cmps", "cmp"])
  -- , (KeyLeftShift,      ["lshift", "lshft", "lsft", "shft", "sft"])
  -- , (KeyRightShift,     ["rshift", "rshft", "rsft"])
  -- , (KeyLeftCtrl,       ["lctrl", "lctl", "ctl"])
  -- , (KeyRightCtrl,      ["rctrl", "rctl"])
  -- , (KeyLeftMeta,       ["lmeta", "lmet", "met"])
  -- , (KeyRightMeta,      ["rmeta", "rmet"])
  -- , (KeyBackspace,      ["bks", "bspc"])
  -- , (KeyCapsLock,       ["caps"])
  -- , (KeyGrave,          ["grv"])
  -- , (Key102nd,          ["102d"])
  -- , (KeyForward,        ["fwd"])
  -- , (KeyScrollLock,     ["scrlck", "slck"])
  -- , (KeyPrint,          ["prnt"])
  -- , (KeyWakeUp,         ["wkup"])
  -- , (KeyLeft,           ["lft"])
  -- , (KeyRight,          ["rght"])
  -- , (KeyLeftBrace,      ["lbrc", "["])
  -- , (KeyRightBrace,     ["rbrc", "]"])
  -- , (KeySemicolon,      ["scln", ";"])
  -- , (KeyApostrophe,     ["apos", "'"])
  -- , (KeyGrave,          ["grv", "`"])
  -- , (KeyBackslash,      ["bksl", "\\"]) -- NOTE: "\\" here is a 1char string, the first \ is consumed by Haskell as an escape character
  -- , (KeyComma,          ["comm", ","])
  -- , (KeyDot,            ["."])
  -- , (KeySlash,          ["/"])
  -- , (KeyNumLock,        ["nlck"])
  -- , (KeyKpSlash,        ["kp/"])
  -- , (KeyKpEnter,        ["kprt"])
  -- , (KeyKpPlus,         ["kp+"])
  -- , (KeyKpAsterisk,     ["kp*"])
  -- , (KeyKpMinus,        ["kp-"])
  -- , (KeyKpDot,          ["kp."])
  -- , (KeySysRq,          ["ssrq", "sys"])
  -- , (KeyIso,            ["iso"])
  -- , (KeyKbdIllumDown,   ["bldn"])
  -- , (KeyKbdIllumUp,     ["blup"])
  -- , (KeyNextSong,       ["next"])
  -- , (KeyPlayPause,      ["pp"])
  -- , (KeyPreviousSong,   ["prev"])
  ]
