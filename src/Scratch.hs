{-# OPTIONS_GHC -Wno-all #-}
module Scratch

where

import KMonad.Prelude

import Control.Exception.Safe
import Control.Exception.Lens

data Err
  = ErrA Int
  | ErrB Char
  deriving Show
makeClassyPrisms ''Err

instance Exception Err

-- class AsErr t where _Err :: Prism' t Err
-- instance AsErr Err where _Err = id
instance AsErr SomeException where _Err = exception

-- _ErrA :: AsErr t => Prism' t Int
-- _ErrA = _Err . prism' ErrA f where
--   f (ErrA i) = Just i
--   f _        = Nothing

-- _ErrB :: AsErr t => Prism' t Char
-- _ErrB = _Err . prism' ErrB f where
--   f (ErrB c) = Just c
--   f _        = Nothing

data Nest
  = NestErr Err
  | NestChar Char
  deriving Show
makeClassyPrisms ''Nest

instance Exception Nest
instance AsNest SomeException where _Nest = exception
instance AsErr Nest where _Err = _NestErr

-- instance AsNest Err where _Nest = prism'

-- | Throwing
foo :: OnlyIO ()
foo = throwing _ErrA 3

-- | Catching the literal error
bar :: OnlyIO ()
bar = catching _ErrA foo $ \e -> q "caught" >> q e

-- | Catching the class of errors
bam :: OnlyIO ()
bam = catching _Err foo $ \e -> q "caught" >> q e

-- | Catching nested <- doesn't work
bop :: OnlyIO ()
bop = catching _Nest foo $ \e -> q "caught" >> q e

-- -- | Cathcing nested while throwing low
-- bip :: OnlyIO ()
-- bip = catching _Nest foo $ \e -> q "caught" >> q e

-- | Throwing normal and catching nested


-- _ErrA = _Err . ErrA

-- data ErrA = ErrA Int  deriving Show
-- makeClassyPrisms ''ErrA
-- data ErrB = ErrB Char deriving Show
-- makeClassyPrisms ''ErrB

-- instance Exception ErrA
-- instance Exception ErrB

-- instance AsErrA SomeException where _ErrA = exception

-- instance AsErrB SomeException where _ErrB = exception

-- $err

-- data IOCtlGrabErr    = IOCtlGrabErr    FilePath     FailCode deriving Show
-- makeClassyPrisms ''IOCtlGrabErr
-- data IOCtlReleaseErr = IOCtlReleaseErr FilePath     FailCode deriving Show
-- makeClassyPrisms ''IOCtlReleaseErr
-- data EventDecodeErr  = EventDecodeErr  B.ByteString Text     deriving Show
-- makeClassyPrisms ''EventDecodeErr
-- data InvalidEventErr = InvalidEventErr Int32        Word16   deriving Show
-- makeClassyPrisms ''InvalidEventErr

-- instance AsIOCtlGrabErr SomeException where _IOCtlGrabErr = exception

-- data EvdevErr
--   = IOCtlGrabErrT    IOCtlGrabErr
--   | IOCtlReleaseErrT IOCtlReleaseErr
--   | EventDecodeErrT  EventDecodeErr
--   | InvalidEventErrT InvalidEventErr
-- makeClassyPrisms ''EvdevErr
