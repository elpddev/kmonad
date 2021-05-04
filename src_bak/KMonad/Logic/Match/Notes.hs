{-# LANGUAGE GADTs #-}
module KMonad.Logic.Match.Notes

where

import KMonad.Prelude
import KMonad.Prelude.Example

import Control.Comonad

import KMonad.Logic.Stream hiding (take)
import qualified KMonad.Logic.Stream as P


data Match a where
  Match :: Eq a => a -> Match a

instance Show a => Show (Match a) where
  show (Match a) = "(Match " <> show a <> ")"
