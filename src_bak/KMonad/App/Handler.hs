{-# LANGUAGE ScopedTypeVariables #-}
module KMonad.App.Handler

where

import KMonad.Prelude hiding (Handler, null)

import System.IO

import qualified RIO.HashMap as M
import qualified RIO.Set     as S
import RIO.List (elemIndex)
import RIO.List.Partial (tail)
-- import RIO.NonEmpty (nonEmpty)

newtype Match a = Match { runMatch :: a -> Bool }

anything, nothing:: Match a
anything = Match  $ const True
nothing  = Match $ const False

mor :: Match a -> Match a -> Match a
mor = undefined

mand :: Match a -> Match a -> Match a
mand = undefined

match :: Eq a => a -> Match a
match = Match . (==)

matchWith :: (a -> Bool) -> Match a
matchWith = Match

-- | A handler is:
--
-- A function that is called on some input element
-- That monadically produces:
--   Maybe a new handler to keep on the stack
--   A possibly empty list of elements to call downstream-handlers on, LIFO
-- newtype Handler m a = Handler { unHandler :: a -> m (Maybe (Handler m a), [a]) }
-- The above produces a handler construct that is more powerful than required,
--
-- ^^^ orrrrr:

-- | Basic handler type
-- 1. We trigger you with an a
-- 2. You get some monadic action
-- 3. To produce the function to call on the next a

-- type Q a m = a -> m (a -> m (a -> m ....)) -- Aaaaaaahhhh, this is recursive
newtype Q m a = Q { runQ :: a -> m (Q m a) }
-- ^ Every `Q` can be run on an `a`, when that is done we produce the next Q to
-- be run on an `a`. All the memory and branching stuff is just in how we
-- combine functions.

-- ^ This is super general, but we also never need a collection of these
-- functions, we always have exactly 1 such function, it is guaranteed to create
-- its own successor, and it gets an 'm' to do a lot of cool stuff with.
--
-- Then the (Match a, m ()) is an implementation of `Q a m` that is less
-- general, but more specific.

mapQ :: Monad m => Q m a -> [a] -> m (Q m a)
mapQ q []     = pure q
mapQ q (a:as) = runQ q a >>= \q' -> mapQ q' as

-- data Handler' m a = Handler'
--   { _match  :: Match a
--   , _action :: m ()
--   }

bind :: (Eq a, Monad m) => a -> m () -> Q m a
bind tgt go = Q $ \a -> do
  when (tgt == a) go
  pure (bind tgt go)

-- Should be careful, think I am mixing binding to tgt and the emit-loop. Can
-- probably make bind-to-tgt a separate abstraction
emit :: (Eq a, HasOut b m) => b -> a -> Q m a
emit c tgt = Q $ \a -> do
  when (tgt == a) (putout c)
  pure (emit c tgt)

-- Party-trick to show how to write closure around state
emitCount :: forall a b m. (Eq a, HasOut b m, Show b) => b -> a -> Q m a
emitCount c tgt = Q (go 0)
  where
    go :: Int -> a -> m (Q m a)
    go n a = if tgt == a
          then do
            traceShowM n
            putout c
            pure $ if n < 10 then Q (go $ n + 1)
                             else Q (go $ 0)
          else do
            pure $ Q (go n)

-- Create a Q by reapplying the same monadic function
redo :: Monad m => (a -> m ()) -> Q m a
-- redo f = Q $ \a -> f a >> pure (redo f)
redo f = cycle $ f :| []



null :: Applicative m => Q m a
null = Q $ const $ pure null

cycle :: forall a m. Monad m => NonEmpty (a -> m ()) -> Q m a
cycle fs = Q $ go l
  where
    l :: [a -> m ()]
    l = toList fs
   
    go :: [a -> m ()] -> a -> m (Q m a)
    go (f:[])  a = f a >> pure (Q $ go l)
    go (f:rst) a = f a >> pure (Q $ go rst)

-- | Note: we use a map internally, duplicate keys will be overwritten, no order
-- guarantees.
fromList :: (Ord a, Hashable a, Monad m) => [(a, Q m a)] -> Q m a
fromList = fromMap . M.fromList

fromMap :: (Ord a, Hashable a, Monad m) => M.HashMap a (Q m a) -> Q m a
fromMap m = Q $ \a -> case M.lookup a m of
  Nothing -> pure $ fromMap m
  Just q  -> runQ q a >>= \q' -> pure $ fromMap (M.insert a q' m)

-- | Challenge excepted, but Foldable generalization comes later
pyramid :: forall a. (Eq a) => [a] -> [a]
pyramid = go [] [] [] where
  go :: [a] -- Accumulator for output
     -> [a] -- Storing encounter history
     -> [a] -- Storing remembered releases
     -> [a] -- The input signal
     -> [a] -- Result
  go acc _ rs [] = reverse acc
  go acc h rs (a:as) =
    case elemIndex a h of
      Nothing -> go (a:acc) (a:h) rs as
      Just 0  -> go (a:acc) (tail h) rs as
      -- Just n  -> go acc

-- | Actually, pyramid is overkill, all I need is an:

-- aroundPairs function that makes any handler behave like its input follows the
-- form: 1 a b c b 1 a k c l -> 1 a b c b a c 1 k l
--
-- i.e., it rearranges the input signal so that all elements between 2 signal
-- elements (here 1) come in pairs (or n * 2s). The moment the closing signal is
-- given, it is delayed until all the contents have paired up. Any new elements
-- are put behind the 1.
--
-- Maybe I can write some 'rewrite type' like: Signal a -> Signal a, since all
-- the aroundPairs does is rearrange the input, it cares nothing about what gets
-- done with the signal afterward (we apply a handler to it).


elemPop :: (Eq a) => a -> [a] -> Maybe (a, [a])
elemPop = findPop . (==)

findPop :: (a -> Bool) -> [a] -> Maybe (a, [a])
findPop p = go [] where
  go _   []     = Nothing
  go acc (a:as) = if p a then Just (a, reverse acc <> as) else go (a:acc) as

pyrtest :: [Int]
pyrtest = pyramid [1, 2, 1, 2]

  
    -- | a `notElem` h -> a : (go (a:h) as)
    -- | a == head h   -> a : (go )
--------------------------------------------------------------------------------
-- little test values

class Monad m => HasOut b m | m -> b where
  putout :: b -> m ()

instance HasOut Text IO where
  putout = print

type QIO = Q IO Int


-- NOTE: this way I can hide the function that is supposed to close a button by
-- closing the layer that contains the button first, and there is no way of
-- knowing in what state this button needs to be.
--
-- i.e. If we constrain ourselves only to on-off, then we could 'release all
-- buttons' upon closing a layer, but then we are constraining what is possible.
--
-- Oooorrr... we only allow direct matching against fixed values.

-- That way we could even just let a handler inject a "Elapsed 50", because if 2
-- handlers inject the same wait, they will be able to handle eachothers waits,
-- so we will then handle 2 waits as well. So we should also remember the order-time
--
-- Oooorrrr.. we make a Q very general, but force (or assume) key-signals to
-- always be binary, and then let maps make sure to keep track of closing-functions.
thing :: QIO
thing = fromList $
  [ (3, cycle $ fmap const $ putout "a" :| [putout "b", putout "c"])
  , (4, redo $ const $ putout "haha")
  , (5, cycle $ fmap const $ putout "queque" :| [ putout "boobs!"])
  ]

-- If I want to implement 'first', I will need to be able to signal some form of
-- success

flop :: QIO
flop = cycle $ fmap const $ putout "flip" :| [putout "flop"]

boop :: Q IO Int
boop = bind 3 (putout "bound")

bap :: QIO
bap = redo (\a -> when (a < 5) (putout . tshow $ a))


test :: Q IO Int
test = emit "hello" 3

test2 :: Q IO Int
test2 = emitCount "hello" 3




-- -- testing shorthand
-- type H = Handler' IO Int ()
-- type M = Match Char
-- type Signal = String
