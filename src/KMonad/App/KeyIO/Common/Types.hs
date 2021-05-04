module KMonad.App.KeyIO.Common.Types

where

import KMonad.Prelude
import KMonad.Object.Name
import KMonad.Object.Time

import KMonad.App.Logging
import KMonad.App.KeyIO.Linux.Common as T
import KMonad.App.KeyIO.Windows.Common as T
import KMonad.App.KeyIO.Mac.Common as T

import Control.Exception.Lens

--------------------------------------------------------------------------------
-- $result
--
-- Once all the configuration and initialization is done, this is what the
-- actual _kmonad engine should end up with: an IO action that gets keycodes,
-- and an IO action that writes keycodes somewhere. Once we are up and running,
-- there is no more distinction between OSes or production-vs-testing.
--
-- NOTE: This is not a very common type, it only exists to point at the fact
-- that any OS-specific implementation must define some 'Keycode' type and then
-- plug it into these types. 'GetKey' and 'PutKey' are the actually /completed/
-- types that we work with.

type CanKeycode c = (Eq c, Ord c, Num c, Show c, Enum c, Hashable c)

type GetKey_ c = OnlyIO c
type PutKey_ c = (c -> OnlyIO ())


--------------------------------------------------------------------------------
-- $tokens
--
-- KeyIO tokens are descriptive: they point at what type of KeyIO needs to be
-- done, and what configuration should be passed to the chosen key-io
-- functionality. We separate getting from putting. Furthermore, we separate
-- OS-specific real-world IO from cross-platform testing IO.
--
-- NOTE: The configuration records for an OS-specific KeyIO module must be
-- provided in 'Common', for example: 'EvdevCfg' describes exactly what keyboard
-- to open with the evdev module, and is available on every OS. The way that
-- this record gets handled by the engine differs (it only works on Linux), but
-- the configuration can be parsed and represented on any OS. This way we don't
-- have to do any OS-specific parsing of configuration-files, and people can use
-- 1 config file across different OSes.

-- | ADT describing all possible testing inputs
data TestGetCfg
  = ListGet Text -- ^ A list of events to-be-parsed-from Text

-- | ADT describing all possible testing outputs
data TestPutCfg
  = LogPut       -- ^ Test output that logs events according to LogEnv

-- | ADT describing all possible input configurations
data IOGetCfg
  = EvdevGet EvdevCfg -- ^ Use a linux evdev file with ioctl

-- | ADT describing all possible output configurations
data IOPutCfg
  = UinputPut UinputCfg -- ^ Uses linux 'uinput' kernel module to inject events

type GetCfg = Either TestGetCfg IOGetCfg
type PutCfg = Either TestPutCfg IOPutCfg

--------------------------------------------------------------------------------

-- | IsPress is a boolean value where True indicates a press, and False
-- indicates a release. We simply try to ameliorate some of the boolean
-- blindness by naming the type.
type IsPress = Bool

--------------------------------------------------------------------------------
-- $exc
--

-- | The things that can go wrong with KeyIO. I'd like to have more informative
-- packets than 'Text', but the environments and events are different between
-- OSes, and we basically never try to recover from these exceptions using the
-- information packets that they wrap.
data KioException
  = CouldNotAcquireKio  Text -- ^ Problem trying to acquire resource
  | CouldNotReleaseKio  Text -- ^ Problem trying to relaese resource
  | CouldNotEncodeEvent Text -- ^ Problem encoding an event to be emitted
  | CouldNotDecodeEvent Text -- ^ Problem decoding event from OS
  | KioResourceLost     Text -- ^ Already acquired resource lost
  deriving Show
makeClassyPrisms ''KioException


instance Exception KioException
instance AsKioException SomeException where _KioException = exception
