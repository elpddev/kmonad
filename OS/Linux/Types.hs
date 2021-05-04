module KMonad.App.OS.Linux.Types

where

import KMonad.Prelude

import KMonad.App.OS.Types


-- | Linux keycodes are natively Word16, we provide a newtype wrapper for the
-- typesystem.
newtype Lcode = Lcode { unLcode :: Word16 }
  deriving (Eq, Ord, Num, Show)

-- |
instance Parse Lcode where
  parser = undefined


-- | The RawEvent datatype
--
-- Linux produces a stream of binary data representing all its input events
-- through the \/dev\/input files. Each event is represented by 5 numbers:
-- seconds, microseconds, event-type, event-code, and event-value. For more
-- explanation look at: https://www.kernel.org/doc/Documentation/input/input.txt
--
-- We parse the entire event, but discard everything except the keycode.
data RawEvent = RawEvent
  { _leS    :: !Word64  -- ^ The seconds component of system time
  , _leNS   :: !Word64  -- ^ The nanoseconds component of system time
  , _leType :: !Word16  -- ^ The type signals the kind of event (we only use EV_KEY)
  , _leCode :: !Lcode   -- ^ The keycode indentifier of the key
  , _leVal  :: !Int32   -- ^ Whether a press, release, or repeat event
  } deriving (Show)
makeClassy ''RawEvent

