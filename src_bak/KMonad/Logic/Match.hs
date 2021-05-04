{-# LANGUAGE GADTs #-}
module KMonad.Logic.Match

where

import KMonad.Prelude
import KMonad.Prelude.Example

import Control.Comonad

import KMonad.Logic.Stream hiding (take)
import qualified KMonad.Logic.Stream as P

import RIO.State

data Match a where
  -- | Succeeds, doesn't consume input
  Done   :: Match a
  -- | Succeeds, consumes 1 element
  Any    :: Match a
  -- | Match exactly 1 element
  Lit    :: Eq a => a -> Match a
  -- | Succeeds if 2 matches succeed in sequence
  After  :: Match a -> Match a -> Match a
  -- | Succeed if one of 2 matches succeeds, 1st one is tried first
  Choice :: Match a -> Match a -> Match a

  -- | Match a sequence of matches ended by a match
  --
  -- Will match the 1st Match 0 or more times
  -- Will match the 2nd Match exactly once, and return True
  -- If at any point both matches fail, the entire `Until` fails
  -- Until  :: Match a -> Match a -> Match a

instance Show a => Show (Match a) where
  show Done    = "Done"
  show Any     = "Any"
  show (Lit a) = "Lit " <> show a
  show (After m n) = show m <> " <>> " <> show n
  show (Choice m n) = "Choice (" <> show m <> ") (" <> show n <> ")"

-- | Operator for After
(<>>) :: Match a -> Match a -> Match a
(<>>) = After

-- | Operator for Choice
(<?>) :: Match a -> Match a -> Match a
(<?>) = Choice

-- | Match as 0 or more occurences of 1, then exactly 1 of 2.
--
-- If both matches fail, the entire match fails
until :: Match a -> Match a -> Match a
until m n = n <?> (m <>> until m n)

-- | Create a stream of booleans that is true wherever the beginning of a match
-- starts, that ends up matching.
matches :: Match a -> Stream a -> Stream Bool
matches = extend . runMatch

-- | Run a 'Match' against a 'Stream'
runMatch :: Match a -> Stream a -> Bool
runMatch m = evalState (runMatch' m)

-- | Utility to get the next element and move the state-stream index by 1
pop :: MonadState (Stream a) m => m a
pop = do
  (Cons a as) <- get
  put as
  pure a

runMatch' :: Match a -> State (Stream a) Bool
runMatch' Done        = pure True
runMatch' Any         = True <$ pop
runMatch' (Lit a)     = (a ==) <$> pop
runMatch' (After m n) = runMatch' m >>= \b ->
  if b then runMatch' n else pure False
runMatch' (Choice m n) = runState (runMatch' m) <$> get >>= \(a1, s1) ->
  if a1 then put s1 >> pure True else runMatch' n

--------------------------------------------------------------------------------

testStr :: Stream Int
testStr = periodic $ 1 :| [2, 3, 3, 4, 3, 2]

runTest :: IO m => Int -> Stream Int -> Match Int -> m ()
runTest n s m = do
  putStrLn $ showInts  $ P.take n $ s
  putStrLn $ showBools $ P.take n $ matches m s
  putStrLn ""

testCore :: (IO m) => m ()
testCore = do
  let n = 30
  let go m = putStrLn (tshow m) >> runTest n testStr m
  go $ Done
  go $ Lit 1
  go $ Lit 0
  go $ Lit 1 <>> Lit 2
  go $ Any   <>> Lit 1
  go $ Done  <>> Lit 1
  go $ Lit 1 <>> Done
  go $ Lit 3 <>> Any <>> Lit 2
  go $ Lit 1 <?> Lit 3
  go $ Lit 3 <>> Lit 4
  go $ Lit 1 <?> (Lit 3 <>> Lit 4)

testRecur :: IO m => m ()
testRecur = do
  let n = 60
  let ps = putStrLn
  let go m = runTest n testStr m

  ps $ "until (Lit 2) (Lit 3)"
  go $ until (Lit 2) (Lit 3)

  ps $ "Lit 2 <>> until (Lit 3) (Lit 4)"
  go $ Lit 2 <>> until (Lit 3) (Lit 4)

  -- ps $ unde
