module KMonad.Object.Stream.Operations
 
where

import KMonad.Prelude

import KMonad.Object.Time
import KMonad.Object.Stream.Types

import qualified RIO.List as L
import qualified RIO.Set as S


--------------------------------------------------------------------------------
-- $constructors

-- | Create a stream by applying some function over and over
iterate :: (a -> a) -> a ->  Stream a
iterate f = unsafeFromInf . L.iterate f

-- | Create a stream from some foldable that is supposed to be infinite
--
-- If the foldable terminates we 'throw' an EndOfStream exception
unsafeFromInf :: Foldable t => t a -> Stream a
unsafeFromInf = go . toList where
  go []     = throwing_ _EndOfStreamException
  go (a:as) = (Cons a $ go as)

-- | Turn a finite but non-empty list into a repeating stream
periodic :: NonEmpty a -> Stream a
periodic (a :| as) = unsafeFromInf $ L.cycle (a:as)

--------------------------------------------------------------------------------
-- $manipulators

-- | Pop the first element of a stream
pop :: Stream a -> (a, Stream a)
pop (Cons a rst) = (a, rst)

-- | Add some elements to the front of a stream
prepend :: [a] -> Stream a -> Stream a
prepend [] rst = rst
prepend (a:as) rst = Cons a $ prepend as rst

-- | Pull the first occurence matching some predicate to the front
pull :: Predicate a -> Stream a -> (a, Stream a)
pull p = go [] where
  go acc (Cons a as) | p a       = (a, prepend (reverse acc) as)
                     | otherwise = go (a:acc) as
                    
-- | Pull an element out by it's equality
--
-- (>) and (<) are just as trivial, but boiler-plate
pullElem :: Eq a => a -> Stream a -> (a, Stream a)
pullElem = pull . (==)

-- | Pull out a number of elements in a set
--
-- NOTE: I could make a pullList that is slower, but could pull duplicates
pullSet :: Ord a => Set a -> Stream a -> ([a], Stream a)
pullSet s = go [] [] s where
  go acc fut s (Cons a as)
    | S.null s       = (reverse acc, prepend (reverse fut) $ Cons a as)
    | a `S.member` s = go (a:acc) fut (S.delete a s) as
    | otherwise      = go acc (a:fut) s as


--------------------------------------------------------------------------------
--  $combinators

merge
  :: (a -> b -> Either a b) -- ^ How to choose between 2 elements
  -> Stream a               -- ^ A stream of a elements
  -> Stream b               -- ^ A stream of b elements
  -> Stream (Either a b)    -- ^ The merged stream
merge f (Cons a as) (Cons b bs) = let e = f a b in Cons e . uncurry (merge f) $
  case e of Left _ -> (as, Cons b bs)
            Right _ -> (Cons a as, bs)

--------------------------------------------------------------------------------
-- $timed
--
-- Special operations for streams of things that have times

-- | Try to get the head of a stream, but do this with some timeout, so that if
-- whatever is generating the stream hasn't created a new value in some time, we
-- stop trying and return Nothing
popWithin :: (UIO m)
  => Stream a
  -> Ms
  -> m (Maybe (a, Stream a))
popWithin s d = within d (pure $ pop s)

-- | Look at the time-value of some object in a stream and see pop it off if it
-- occured before some moment in time.
popPeek :: (HasTime a)
  => Stream a              -- ^ The Stream to read from
  -> Time                  -- ^ The point at which to timeout
  -> (Maybe (a, Stream a)) -- ^ The next event, popped from the stream,
                           --   if it occured before timeout
popPeek (Cons a rest) t | a^.time <= t = Just (a, rest)
                        | otherwise    = Nothing
