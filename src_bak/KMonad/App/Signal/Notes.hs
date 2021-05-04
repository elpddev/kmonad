{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module KMonad.App.Signal.Notes

where

import KMonad.Prelude
import KMonad.Prelude.Example

import Control.Comonad

import KMonad.Core
import KMonad.App.Types
import KMonad.App.Signal


nats :: Stream Int
nats = go 1
  where go n = Cons n (go $ n + 1)

takeS :: Int -> Stream a -> [a]
takeS n = take n . toList

sumS :: Num a => Int -> Stream a -> a
sumS n = sum . takeS n --if n <= 0 then 0 else a + sumS (n - 1) as

prodS :: Num a => Int -> Stream a -> a
prodS n = product . takeS n

average :: Fractional a => Int -> Stream a -> a
average n s = sumS n s / fromIntegral n

sums :: Int -> Stream Int -> Stream Int
sums n = extend (sumS n)

prods :: Int -> Stream Int -> Stream Int
prods n = extend (prodS n)

foos :: Int -> Stream Int -> Text
foos n = foldMap tshow . takeS n

foo :: Int -> Stream Int -> Stream Text
foo n = extend (foos n)


-- | First implementatino
match' :: (Comonad w, Eq a) => a -> w a -> w Bool
match' a = extend (\w -> a == extract w)

-- | Better
-- match :: (Comonad w, Eq a) => a -> w a -> w Bool
-- match a = liftW (a ==)

subseq :: Eq a => [a] -> Stream a -> Stream Bool
subseq sub s = extend (go sub) s where
  go [] _ = True
  go (a:as) (Cons b bs)  = (a == b) && go as bs


is3 :: Stream Bool
is3 = runMatch (match 3) nats

albert :: Stream Char
albert = unsafeFromList "abcba"

bart :: Stream Char
bart = unsafeFromList "aaabscraastfxasraswaaasaaasa"

test1 = takeS 5 $ runMatch (match 'a') $ albert

test2 = takeS 30 $ subseq "aa" $ bart



-- So.. decisions are just a tree, no?

-- Sooooo, I should just write my matcher as some sort of applicative parser,
-- should be very simple.

newtype Match a = Match { unMatch :: Stream a -> (Bool, Stream a) }

-- Match also seems a Monoid, or actually, 2 right?

runMatch :: Eq a => Match a -> Stream a -> Stream Bool
runMatch m = extend (fst . unMatch m)

match :: Eq a => a -> Match a
match it = Match $ \(Cons a as) -> (a == it, as)

anyM :: Match a
anyM  = Match $ \(Cons _ as) -> (True, as)

none :: Match a
none = Match $ \s -> (False, s)

after :: Match a -> Match a -> Match a
after p q = Match $ \s -> case unMatch p s of
  (False, _)  -> (False, s)
  (True, rst) -> case unMatch q rst of
    (False, _)  -> (False, s)
    (True, fut) -> (True, fut)

instance Semigroup (Match a) where
  a <> b = after a b

testStr :: String
testStr = "aabbccabcabcaaabbbccc"

prtB :: IO m => [Bool] -> m ()
prtB = print . map (bool '.' 'T')

runTest :: IO m => Match Char -> m ()
runTest m = prtB $ takeS 20 $ runMatch m $ unsafeFromList testStr


ab :: Match Char
ab = match 'a' <> match 'b'

xa :: Match Char
xa = anyM <> match 'a'

ax :: Match Char
ax = match 'a' <> anyM

cab :: Match Char
cab = match 'c' <> match 'a' <> match 'b'

bxxc :: Match Char
bxxc = match 'b' <> anyM <> anyM <> match 'c'



-- any :: (Comonad w) => w a -> Bool
-- any = const True

-- none :: (Comonad w) => w a -> Bool
-- none = const False

-- after :: (w a -> Bool) -> (w a -> Bool) -> (w a -> Bool)
-- after a b

-- data Match a
--   = Any
--   | None
--   | Lit a
--   | After  (Match a) (Match a)
--   | Choice (Match a) (Match a)
--   deriving Show

-- -- FIXME: remove show a
-- runMatch :: forall a t. (Show a, Eq a, Foldable t) => Match a -> t a -> Bool
-- runMatch match stream = fst $ go match (toList stream) where
--   go :: Match a -> [a] -> (Bool, [a])
--   go _ []          = (False, [])
--   go Any  _         = (True, [])
--   go None _       = (False, [])
--   go (Lit b) (a:as) = (a == b, as)
--   go (After p q) as =
--     let (b, rst) = traceShow p $ go p as
--     in if b then go q rst else (False, [])

-- afoo = extend (runMatch Any) albert
-- abar = extend (runMatch None) albert
-- abaz = extend (runMatch (Lit 'a')) albert
-- afor = extend (runMatch (After Any Any)) albert
-- -- abar = runMatch (After Any (Lit 'b')) ("abcd" :: String)
