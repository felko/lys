-- | Representation of parse errors
module Language.Pion.Parser.Error
  ( ParseError (..),
    ParseErrorRepr,
    reprParseErrorBundle,
  )
where

import Data.Some
import Language.Pion.Lexer.Token
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle)
import qualified Text.Megaparsec as Mega

type ParseErrorRepr = Doc AnsiStyle

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

reprParseErrorBundle :: Mega.ParseErrorBundle TokenStream ParseError -> ParseErrorRepr
reprParseErrorBundle = fromString . Mega.errorBundlePretty
