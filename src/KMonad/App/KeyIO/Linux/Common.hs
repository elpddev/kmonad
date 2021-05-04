module KMonad.App.KeyIO.Linux.Common

where

import KMonad.Prelude

import KMonad.Object.Name


--------------------------------------------------------------------------------
-- $evdev

-- | The configuration record for evdev key-input
data EvdevCfg = EvdevCfg
  { _pth :: FilePath -- ^ The path to the input-file to open and capture
  }
makeClassy ''EvdevCfg

--------------------------------------------------------------------------------
-- $uinput

-- | Configuration of the Uinput keyboard to instantiate
data UinputCfg = UinputCfg
  { _vendorCode     :: !Int  -- ^ USB vendor code of the generated keyboard
  , _productCode    :: !Int  -- ^ USB product code of the generated keyboard
  , _productVersion :: !Int  -- ^ USB product version
  , _keyboardName   :: !Name -- ^ Name used to identify keyboard to OS
  , _preInit        :: !(Maybe String)
    -- ^ Optionally, a command to run before trying to open a uinput keyboard
  , _postInit       :: !(Maybe String)
    -- ^ Optionally, a command to execute after keyboard has been generated
  } deriving (Eq, Show)
makeClassy ''UinputCfg

-- | The default uinput configuration
instance Default UinputCfg where
  def = UinputCfg
    { _vendorCode     = 0xFFFF
    , _productCode    = 0xFFFF
    , _productVersion = 0x0000
    , _keyboardName   = "KMonad simulated keyboard"
    , _preInit        = Just "/run/wrappers/bin/sudo /run/current-system/sw/bin/modprobe uinput && /run/current-system/sw/bin/sleep 0.5"
    -- , _preInit        = Nothing
    , _postInit       = Nothing
    }
