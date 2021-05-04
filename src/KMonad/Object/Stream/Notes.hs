module KMonad.Object.Stream.Notes

where

import KMonad.Prelude

import KMonad.Object.Stream

import qualified RIO.List as L
import qualified RIO.Set as S

-- Sort of a scratch-pad of stream expressions and how they can be used.


-- | The stream of 'forever 1'
one :: Stream Int
one = pure 1

-- | The stream of natural numbers
nats :: Stream Int
nats = iterate (+1) 1

-- | The stream of +1, +2, +3
adds :: Stream (Int -> Int)
adds = (+) <$> nats

-- | Repeat counting to n
count :: Int -> Stream Int
count n | n < 2     = error "Don't be foolish"
        | otherwise = periodic $ 1 :| [2..n]

-- | The stream of numbers added to themselves
--
-- ofc: dubs = (+) <$> nats <*> nats
dubs :: Stream Int
dubs = adds <*> nats

-- | Some arbitrary numbers in a set
arby :: S.Set Int
arby = S.fromList [2, 3, 5, 8]

smaller :: Int -> Int -> Either Int Int
smaller a b = if a < b then Left a else Right b

-- | Some example of operations on streams
ex1 :: UIO m => m ()
ex1 = do
  -- How pullSet can pull elements to the front of a stream
  q $ uncurry prepend $ pullSet arby nats

  -- How merge can merge 2 streams by whichever element is smaller
  q $ merge smaller nats dubs

  -- How withStreamIO can run IO-actions as streams
  tick <- metronome 1
  withStreamIO tick $ \s -> q s
