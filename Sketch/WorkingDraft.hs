module Sketch.Types

where

import KMonad.Prelude
import System.IO

import Data.Unique

import Data.Time.Calendar.OrdinalDate

import RIO.State

{- # Separate logic into Engine (IO) and Model (pure)

# Engine

The engine needs to take care of:
- Producing the next event
- Handling output events

## Execution
The execution engine is 1 engine, it:
- Reads events from the OS
- Writes keyboard events to the OS
- Keeps track of timer events by pausing
<- Has 3 versions, 1 for each OS.

## Test
The test engine is 1 engine, it:
- Reads events from a prepared list
- Writes events to a log
- Keeps track of timer events by knowing the future
<- Only supports a limited set of keycodes

# Model

The model is a stateful but otherwise pure computation that translates events
into state updates and sequences of output events.

-}



-- | Placeholder
data Event a = Event
  { _code :: a }
  deriving (Eq, Show)


class Eq a => CanKeycode a where
instance CanKeycode Int
instance CanKeycode Char


-- | Class of things that can function as an engine
class (CanKeycode (Keycode a), Monad m) => CanEngine m a where
  type Keycode a :: *
  getEvent :: a -> m (Event (Keycode a))
  putEvent :: a -> (Event (Keycode a)) -> m ()

-- | Placeholder
data Example = Example
  { _foo :: Text
  , _bar :: Char
  } deriving (Eq, Show)
makeLenses ''Example

-- | Make the placeholder an instance of CanEngine
instance CanEngine IO Example where
  type Keycode Example = Int
  getEvent _   = pure (Event 3)
  putEvent _ e = print e

class Monad m => CanModel m c a where
  step :: a -> (Event c) -> m [Event c]

data SomeModel = SomeModel
  { _baz :: Int }

instance CanModel IO c SomeModel where
  step _ = pure . (:[])


run :: (CanEngine m e, CanModel m (Keycode e) a) => e -> a -> m ()
run e a = do
  i  <- getEvent e
  os <- step a i
  mapM_ (putEvent e) os

test :: IO ()
test = do
  let it = Example "hello" 'c'
  let ut = SomeModel 3
  run it ut


  -- mapM_ (putEvent e) (getEvent e >>= handle a)

-- | Code to make stuff run
-- test :: IO ()
-- test = do
--   let it = Example "hello" 'c'
--   e <- getEvent it
--   putEvent it e


-- data Engine = Engine
--   { getNow :: IO a
--   , getNextEvent :: IO
--   }

-- newtype Tag  = Tag Unique
--   deriving (Eq, Ord)

-- instance Show Tag where
--   show (Tag u) = show . hashUnique $ u

-- -- type Time = UTCTime
-- type Time = Int

-- data Event m
--   = TimeEvent UTCTime Tag
--   | KeyEvent  UTCTime (Keycode m)

-- instance Eq m => Eq (Event m) where
--   _ == _ = true
-- -- instance Eq a => Eq (Event a) where
-- --   (TimeEvent t1 g1) == (TimeEvent t2 g2) = t1 == t2 && g1 == g2

-- -- type KC = forall a. Keycode a

-- class Monad m => CanTime m where
--   getNow :: m UTCTime

-- -- class (Eq, Show a) => CanKeycode a where



-- class (Eq c, CanTime m) => CanEngine c m where
--   type Keycode :: c
--   -- type Keycode m :: *
--   -- getNow   :: IO a
--   getEvent  :: m (Event m)
--   putEvent  :: Event m -> m ()

-- instance CanTime IO where
--   getNow = getCurrentTime

-- instance CanEngine IO where
--   type Keycode IO = Int
--   getEvent = pure $ KeyEvent foo x -- <$> getNow
--   putEvent _ = print "arst"

-- x :: Keycode IO
-- x = 3

-- -- class Monad m => CanModel m where
-- --   step :: Event -> m [Event]


-- foo :: UTCTime
-- foo = UTCTime (fromOrdinalDate 2020 23) 0
