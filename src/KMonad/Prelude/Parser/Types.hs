module KMonad.Prelude.Parser.Types

where

import RIO

import Text.Megaparsec hiding (ParseError)
import qualified Text.Megaparsec.Char.Lexer as L

{- The _kmonad wide parser configuration -}




--------------------------------------------------------------------------------
-- $bsc
--
-- The basic types of parsing

-- | Parser's operate on Text and carry no state
type Parser = Parsec Void Text


--------------------------------------------------------------------------------
-- $errs

-- | The type of errors returned by the Megaparsec parsers
newtype ParseError = ParseError (ParseErrorBundle Text Void)

-- | Use the errorBundlePretty to display ParseError
instance Show ParseError where
  show (ParseError e) = "Parse error at " <> errorBundlePretty e

-- | Allow for ParseError to be thrown as exceptions
instance Exception ParseError

--------------------------------------------------------------------------------
-- $class

-- | Instances of this class have a defined parser
class Parse a where parser :: Parser a


instance Parse Char where parser = anySingle
instance Parse Int  where parser = L.decimal
