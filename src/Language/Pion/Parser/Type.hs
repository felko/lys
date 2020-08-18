-- | Parser for types and sequents.
module Language.Pion.Parser.Type
  ( type',
    composite,
    modality,
    unit,
    sequent,
    context,
  )
where

import qualified Language.Pion.Lexer.Token as Token
import Language.Pion.Parser.Monad
import Language.Pion.SourceSpan
import Language.Pion.Syntax.Type

type' :: Parser (Located Type)
type' =
  located
    ( composite
        <|> unit
        <|> modality
        <|> variable
        <?> "type"
    )

composite :: Parser Type
composite =
  Composite
    <$> connectiveType
    <*> conjunction
      Token.Brace
      Token.Brack
      Token.Colon
      type'

unit :: Parser Type
unit = Unit <$> unitType

modality :: Parser Type
modality =
  Modality
    <$> modalityType
    <*> type'

variable :: Parser Type
variable = Variable <$> identifier

context :: Parser Context
context = branch Token.Colon (located name) type' `sepBy` Token.Comma

sequent :: Parser (Located Sequent)
sequent = located do
  sequentAntecedents <- context
  punctuation Token.Turnstile
  sequentSuccedents <- context
  pure Sequent {..}
