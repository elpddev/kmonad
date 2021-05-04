module KMonad.Prelude.Example
  ( print
  , putStrLn
  , printList
  , (!!)
  , showInts
  , showBools
  )
where

import KMonad.Prelude

import RIO.List.Partial ((!!))

import qualified System.IO as SIO

print :: (IO m, Show a) => a -> m ()
print = liftIO . SIO.print

putStrLn :: (IO m) => Text -> m ()
putStrLn = liftIO . SIO.putStrLn . unpack

printList :: (Show a, IO m) => [a] -> m ()
printList = mapM_ print

-- | Show a foldable of ints by smooshing them together, , only useful if all in
--   [0,9]. We use this for making tests easier to read
showInts :: (Foldable t) => t Int -> Text
showInts = foldMap tshow

-- | Show a foldable of bools by making True be T and False be .
showBools :: (Foldable t) => t Bool -> Text
showBools = foldMap (bool "." "T")
