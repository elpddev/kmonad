{-# LANGUAGE AllowAmbiguousTypes, GADTs #-}
{-|
Module      : KMonad.Keyboard.Types
Description : Basic keyboard types
Copyright   : (c) David Janssen, 2021
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

This module contains the basic types involved in representing key-events.
Additionally, since keycodes differ between OSes, this module also contains the
class defining everything an OS-implementation needs to offer (including some
Keycode type).

-}
module KMonad.Keyboard.Types
  ( -- * $switch
    Switch(..)
  , _Pressed
  , HasSwitch(..)

    -- * $os
  , CanKIO(..)
  , GetKey
  , PutKey
  , HasKeycode(..)

    -- * $event
  , KeyEvent(..)
  )
where

import KMonad.Prelude

-- TEMP:
import System.IO

--------------------------------------------------------------------------------
-- $switch
--
-- We use the 'Switch' datatype to indicate state-changes in keys. This is
-- exactly the same as a 'Bool' value, but with some extra clarity, type-safety,
-- and utilities.

-- | Either the activation or deactivation of some switch
data Switch = Press | Release deriving (Eq, Show)

-- | An 'Iso' between 'Bool' and 'Switch' where 'Press' evaluates to 'True'
_Pressed :: Iso' Switch Bool
_Pressed = iso (Press ==) (bool Release Press)

-- | A class describing how to access some value's 'Switch'
class HasSwitch a where switch :: Lens' a Switch

-- | Hooks the base 'Switch' type into 'HasSwitch'
instance HasSwitch Switch where switch = id

-- | How to pretty-print a 'Switch'
instance Display Switch where textDisplay = tshow

--------------------------------------------------------------------------------
-- $os
--
-- Instead of defining a 'Keycode' type, we defer the exact implementation of
-- 'Keycode' to the OS-specific code. Here we define the type-class that any
-- OS-implementation must support, including an associated 'Keycode' type (using
-- TypeFamilies)
--
-- A bit of explanation:
--
-- This was an area where we struggled a bit with the design, hesitating between
-- two approaches:
--
-- 1. Stick to simple Haskell and force the OS-specific implementation to follow
-- a particular naming scheme. I.e. all OS-specific code *has* to provide a
-- 'Keycode' type named exactly such, etc. The benefit is not having to use more
-- arcane extensions, but the downside is that the interface is implicit.
--
-- 2. Use TypeFamilies (and a GADT) to provide an interface that any
-- implementation could use, as long as they could specify an instance of
-- 'CanOS'. The downside is that we include more arcane Haskell, but the actual
-- implementation and interface becomes clearer and explicit.
--
-- Later down the line we choose for option 2, however, I would like to mention
-- here that it is suggested that, if you are starting out, you stick to option
-- 1 and just write a lot of comments detailing exactly what is going on.

-- | For something to function as a 'Keycode' we must be able to compare it for
-- equality and show it (for logging purposes).
type CanKeycode c = (Eq c, Show c)

type GetKey os m = m (KeyEvent (Keycode os))
type PutKey os m = KeyEvent (Keycode os) -> m ()

-- | Typeclass describing what is required to run an OS interface
class (CanKeycode (Keycode os), Monad os, Monad m) => CanKIO os m where
 
  -- | The keycode type for this OS
  type Keycode os :: *

  -- | How to run an action that requires the capability to get and put key events
  withKIO :: (GetKey os m -> PutKey os m -> m a) -> os (m a)
 
  -- -- | The action that returns the next event
  -- getKey :: os (KeyEvent (Keycode os))
  -- -- | The action that dispatches generated events
  -- putKey :: KeyEvent (Keycode os) -> os ()
  -- -- | Perform an action in the context of an acquired keyboard context
  -- runOS  :: os a -> os a

-- | Typeclass describing how to access a 'Keycode' inside some structure
class CanKeycode c => HasKeycode a c where
  keycode :: Lens' a c

-- NOTE: I would like to implement: HasKeycode os (Keycode os), but it refuses
-- with:
--  • Illegal type synonym family application in instance: Keycode os
--  • In the instance declaration for ‘HasKeycode os (Keycode os)’

--------------------------------------------------------------------------------
-- $event
--
-- A 'KeyEvent' is the packet of data describing exactly 1 manipulation of a
-- keyboard. It contains the 'Switch', some identifying 'Keycode', and the time
-- at which the 'KeyEvent' occured.
--
-- Since the 'Keycode' type depends on the OS we have to provide this as an
-- argument to the 'KeyEvent' type. To ensure that we can only construct valid
-- 'KeyEvent's we use a GADT to add a 'CanOS' constraint to the constructor.
--
-- Because we use a GADT (and because 'Keycode' is not a fully resolved type) we
-- also can't derive certain standard instances, but provide implementations
-- here.
--
-- Finally, KMonad uses more events than just keyboard events, but we try to
-- keep concerns separated. Therefore we implement all the raw keyboard related
-- semantics in the "KMonad.Keyboard" module. The "KMonad.Engine" module
-- specifies more events, and ends up plugging the 'KeyEvent' into a sum-type.

-- | A 'KeyEvent' encoding the press or release of a keycode at some time.
data KeyEvent c where
  KeyEvent :: CanKeycode c => Switch -> c -> UTCTime -> KeyEvent c

-- | Eq 'KeyEvent's by eqing their contents
instance Eq c => Eq (KeyEvent c) where
  (KeyEvent s c t) == (KeyEvent s' c' t') = s == s' && c == c' && t == t'

-- | Show 'KeyEvent's by showing their contents
instance Show c => Show (KeyEvent c) where
  show (KeyEvent s c t) = "KeyEvent " <> unwords [show s, show c, show t]

-- | How to access the 'Switch' of a 'KeyEvent'
instance HasSwitch (KeyEvent c) where
  switch = let getter (KeyEvent s _ _)   = s
               setter (KeyEvent _ c t) s = KeyEvent s c t
           in lens getter setter

-- | How to access the 'Time' of a 'KeyEvent'
instance HasTime (KeyEvent c) where
  time = let getter (KeyEvent _ _ t)   = t
             setter (KeyEvent s c _) t = KeyEvent s c t
         in lens getter setter

-- | How to access the 'Keycode' of a 'KeyEvent'
instance CanKeycode c => HasKeycode (KeyEvent c) c where
  keycode = let getter (KeyEvent _ c _)   = c
                setter (KeyEvent s _ t) c = KeyEvent s c t
            in lens getter setter










--------------------------------------------------------------------------------
-- | Tinkering stuff
--
-- TODO: delete me eventually

-- data Env = Env
--   { _foo :: Int
--   , _bar :: Text
--   } deriving (Eq, Show)

-- defEnv :: Env
-- defEnv = Env 3 "hello"

-- instance CanOS (RIO Env) where
--   type Keycode (RIO Env) = Int
--   getKey = do
--     t <- getCurrentTime
--     pure $ KeyEvent Press 3 t
--   putKey = liftIO . print
--   runOS = bracket_ (putStrLn "init") (putStrLn "cleanup") . runRIO defEnv

-- go :: RIO Env ()
-- go = do
--   e <- getKey
--   putKey e

-- test :: IO ()
-- test = runOS go
