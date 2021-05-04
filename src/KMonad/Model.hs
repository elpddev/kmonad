module KMonad.Model

where

import KMonad.Prelude
import KMonad.Object.Time

type Timeout = Ms

-- | A model of how to map what might happen to what to do

data Model a b
  = Within Ms      (Model a b) (Model a b)
  | Test   (a -> Bool) (Model a b) (Model a b)
  | Produce b
