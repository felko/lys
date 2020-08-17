-- | Representation of parse errors
module Language.Pion.Parser.Error
  ( ParseError (..),
    ParseErrorRepr,
    ParseErrorBundle,
    reprParseErrorBundle,
  )
where

import Data.Some
import Language.Pion.Lexer.Token
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle)
import qualified Text.Megaparsec as Mega

data ParseError
  = UnexpectedToken (Some Token)
  | ExtraneousClosingDelimiter DelimiterType
  | MissingClosingDelimiter DelimiterType
  deriving (Eq, Ord, Show)

instance Mega.ShowErrorComponent ParseError where
  showErrorComponent = \case
    UnexpectedToken t -> "Invalid identifier: `" <> show t <> "' is a keyword"
    ExtraneousClosingDelimiter delimType -> "Extraneous closing " <> show delimType
    MissingClosingDelimiter delimType -> "Missing closing " <> show delimType

type ParseErrorRepr = Doc AnsiStyle

type ParseErrorBundle = Mega.ParseErrorBundle Stream ParseError

reprParseErrorBundle :: ParseErrorBundle -> ParseErrorRepr
reprParseErrorBundle = fromString . Mega.errorBundlePretty
