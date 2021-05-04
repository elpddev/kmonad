{-|
Module      : KMonad.Keyboard.Linux.Types
Description : The types particular to Linux keyboards
Copyright   : (c) David Janssen, 2021
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KMonad.Keyboard.Linux.Types
  ( LinuxCode, LinuxEvent, RawEvent(..)
  , HasRawEvent(..)
  , _RawEvent
  , sync
  , module KMonad.Keyboard.Types
  )
where

import KMonad.Prelude
import KMonad.Keyboard.Types

-- | Keycodes in Linux are simply 'Word16'
type LinuxCode = Word16

-- | Shortcut to refer to KMonad key-events with the Linux keycode
type LinuxEvent = KeyEvent LinuxCode

-- | The RawEvent datatype
--
-- Linux produces a stream of binary data representing all its input events
-- through the \/dev\/input files. Each event is represented by 5 numbers:
-- seconds, microseconds, event-type, event-code, and event-value. For more
-- explanation look at: https://www.kernel.org/doc/Documentation/input/input.txt
data RawEvent = RawEvent
  { _leS    :: !Word64    -- ^ The seconds component of system time
  , _leNS   :: !Word64    -- ^ The nanoseconds component of system time
  , _leType :: !Word16    -- ^ The type signals the kind of event (we only use EV_KEY)
  , _leCode :: !LinuxCode -- ^ The keycode indentifier of the key
  , _leVal  :: !Int32     -- ^ Whether a press, release, or repeat event
  } deriving (Show)
makeClassy ''RawEvent

-- | Constructor for linux sync events. Whenever you write an event to linux,
-- you need to emit a 'sync' to signal to linux that it should sync all queued
-- updates.
sync :: UTCTime -> RawEvent
sync t = let (MkSystemTime s ns) = t ^. systemTime
         in RawEvent (fi s) (fi ns) 0 0 0

--------------------------------------------------------------------------------
-- $time
--
-- The interface to times inside the 'RawEvent'

-- | Linux representation of SystemTime
type LinuxTime = (Word64, Word64)

-- | Lens to the time-values in a 'RawEvent'
ltime :: Lens' RawEvent LinuxTime
ltime = lens getter setter
  where getter e         = (e^.leS, e^.leNS)
        setter e (s, ns) = e { _leS = s, _leNS = ns}

-- | An 'Iso' between 'UTCTime' and 'LinuxTime'
linuxTime :: Iso' UTCTime (Word64, Word64)
linuxTime = systemTime . (iso s2l l2s)
  where s2l (MkSystemTime s ns) = (fromIntegral s, fromIntegral ns)
        l2s l = MkSystemTime (fromIntegral $ l^._1) (fromIntegral $ l^._2)

-- | An interface to the time-values inside 'RawEvent'
instance HasTime RawEvent where
  time = ltime . from linuxTime


-------------------------------------------------------------------------------
-- $conv
--
-- We only represent a subset of all the possible input events produced by
-- Linux. First of all, we disregard all event types that are not key events, so
-- we quietly ignore all sync and scan events. There other other events that are
-- there to do things like toggle LEDs on your keyboard that we also ignore.
--
-- Furthermore, within the category of KeyEvents, we only register presses and
-- releases, and completely ignore repeat events.
--
-- The correspondence between RawEvents and core KeyEvents can best be read
-- in the above-mentioned documentation, but the quick version is this:
--   Typ:  1 = KeyEvent            (see below)
--         4 = @scancode@ event    (we neither read nor write)
--         0 = 'sync' event        (we don't read, but do generate for writing)
--   Val:  for keys: 0 = Release, 1 = Press, 2 = Repeat
--         for sync: always 0
--   Code: for keys: an Int value corresponding to a keycode
--           see: https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h
--         for sync: always 0

-- | A 'Prism' between 'RawEvent' and 'KeyEvent'
--
-- NOTE: This is a prism because all valid 'KeyEvent's are 'RawEvent's, but
-- not all 'RawEvent's are valid 'KeyEvent's.
_RawEvent :: Prism' RawEvent LinuxEvent
_RawEvent = prism' toRawEvent fromRawEvent


-- | Translate a 'RawEvent' to a kmonad 'KeyEvent'
fromRawEvent :: RawEvent -> Maybe LinuxEvent
fromRawEvent e
  | e^.leType == 1 && e^.leVal == 0 = Just $ KeyEvent Release c t
  | e^.leType == 1 && e^.leVal == 1 = Just $ KeyEvent Press   c t
  | otherwise = Nothing
  where
    c = e^.leCode
    t = (e^.leS, e^.leNS) ^. from linuxTime

-- | Translate kmonad 'KeyEvent' to 'RawEvent'
toRawEvent :: LinuxEvent -> RawEvent
toRawEvent e = RawEvent (fromIntegral s) (fromIntegral ns) 1 c val
  where
    (s, ns) = e^.time.linuxTime
    c       = e^.keycode
    val     = if (e^.switch == Press) then 1 else 0

