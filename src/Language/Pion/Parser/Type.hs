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
import Language.Pion.Syntax.Concrete.Type

-- | Parse a type.
type' :: Parser (Located Type)
type' =
  located
    ( composite
        <|> unit
        <|> modality
        <|> variable
        <?> "type"
    )

-- | Parse a composite type.
-- A composite type is described by a logical connective and then
-- either a list of types or a list of @field : Type@ pairs.
--
-- Examples:
--
--   * Ordered connective
--
-- > ⅋[Int, Char, String]
-- > ⨁[A, B, C, D]
--
--   * Labelled connective
--
-- > ⨂{ x : Int, y : Int }
-- > &{ left : A, right : B }
composite :: Parser Type
composite =
  Composite
    <$> connectiveType
    <*> conjunction
      Token.Brace
      Token.Brack
      Token.Colon
      type'

-- | Parse an unit type.
-- There is a unit type for all connectives: @⨂@, @⅋@, @⨁@ and @&@ have unit
-- types @end@, @⊥@, @void@ and @⊤@ respectively.
--
-- A unit type is equivalent to an empty composite type of the same connective,
-- whether ordered or labelled, e.g. @⊤ = &[] = &{}@. All composite types and unit
-- types are reduced to labelled connectives when desugaring.
unit :: Parser Type
unit = Unit <$> unitType

-- | Parse an exponential type.
-- Exponentials can be either @?@ or @!@, then followed by any type.
modality :: Parser Type
modality =
  Modality
    <$> modalityType
    <*> type'

-- | Parse a type identifier.
-- No distinction is made between quantified type variables and a type name
-- that refers to a type alias.
variable :: Parser Type
variable = Variable <$> identifier

-- | Parse a logical context as a sequence of type annotations of the form
-- @name : Type@ separated by commas.
context :: Parser Context
context = branch Token.Colon (located name) type' `sepBy` Token.Comma

-- | Parse a sequent, denoted by two contexts separated by a turnstile @⊢@.
sequent :: Parser (Located Sequent)
sequent = located do
  sequentAntecedents <- context
  punctuation Token.Turnstile
  sequentSuccedents <- context
  pure Sequent {..}
