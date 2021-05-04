module KMonad.Logic.Match.Notes.Tests

where

import KMonad.Prelude
import KMonad.Prelude.Example

import Control.Comonad

import KMonad.App.Logging

import KMonad.Logic.Stream hiding (take)
import KMonad.Logic.Match hiding (Match, runTest)
import KMonad.Logic.Match.Notes
import qualified KMonad.Logic.Stream as P


-- Running tests, in the end, is also an IO process... Haskell needs to actually
-- be instructed to do it, so I'd better build an interface.

-- If I can't think of anything else interesting to support as a base-env for
-- *all* tests, I should just make the class interface using LUIO.

data TestEnv = TestEnv { _teLogEnv :: LogEnv }
makeClassy ''TestEnv

-- | NOTE: this doesn't work
-- instance HasCfg    TestEnv LogCfg where logCfg = teLogEnv.cfg
instance HasLogEnv TestEnv        where logEnv = teLogEnv

withDefTest :: UIO m => (TestEnv -> m a) -> m a
withDefTest f = withDefLog $ \lenv -> f $ TestEnv lenv

type T a = RIO TestEnv a

runT :: IO m => TestEnv -> T a -> m a
runT = runRIO


runDefT :: UIO m => T a -> m a
runDefT t = withDefTest $ flip runT t

class Test a where
  runTest :: a -> T ()


 

-- Let's just build an implementation of a stream-test, then some more tests,
-- and then generalize when I have some data.

data MatchTest = MatchTest
  { _stream :: Stream Char
  , _match  :: Match Char
  }


-- TODO: CONTINUE HERE, figure out how to nest configs into envs

instance Test MatchTest where
  runTest m = do
    section




foo :: MatchTest
foo = MatchTest "hello" $ Match 'h'

makeClassy ''MatchTest

-- coreTests :: [Test Int]
-- coreTests =

-- run :: IO m => m ()
-- run = withLog defLogCfg $ \env -> do runRIO

-- runTest :: (EnvUIO m env, HasLogEnv env, HasTest env a) => Test a -> m ()
-- runTest t = do
--   say "hello"

-- -- runAll ::

-- -- runTest :: IO m => Int -> Stream Int -> Match Int -> m ()
-- -- runTest n s m = do
-- --   putStrLn $ showInts  $ P.take n $ s
-- --   putStrLn $ showBools $ P.take n $ matches m s
-- --   putStrLn ""

-- -- runTests :: IO m => m ()
-- -- runTests = do
-- --   let n = 50



-- -- testCore :: (IO m) => m ()
-- -- testCore = do
-- --   let n = 30
-- --   let go m = putStrLn (tshow m) >> runTest n testStr m
-- --   go $ Done
-- --   go $ Lit 1
-- --   go $ Lit 0
