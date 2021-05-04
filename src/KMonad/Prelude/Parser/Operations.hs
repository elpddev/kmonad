module KMonad.Prelude.Parser.Operations

where

import RIO

import Control.Exception
import KMonad.Prelude.Parser.Types
import Text.Megaparsec (runParser)

-- | Run a parser and return either the result or the error
parse :: Parser a -> Text -> Either ParseError a
parse p t = case runParser p "" t of
  Left  e -> Left $ ParseError e
  Right a -> Right a

-- | Run a parser and either return the result or throw the error
parseThrow :: Parser a -> Text -> a
parseThrow p t = either throw id $ parse p t

