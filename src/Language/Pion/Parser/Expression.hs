-- | Expression parser.
module Language.Pion.Parser.Expression
  ( expression,
  )
where

import qualified Language.Pion.Lexer.Token as Token
import Language.Pion.Name
import Language.Pion.Parser.Error
import qualified Language.Pion.Parser.Literal as Literal
import Language.Pion.Parser.Monad
import Language.Pion.Parser.Pattern (pattern')
import Language.Pion.SourceSpan
import Language.Pion.Syntax.Expression
import Language.Pion.Syntax.Literal

-- | Parse an expression.
expression :: Parser (Located Expression)
expression =
  abstraction
    <|> application
    <?> "expression"

-- | Parse an expression factor in an application.
factor :: Parser (Located Expression)
factor =
  literal
    <|> tuple
    <|> alternative
    <|> variable
    <|> between Token.Paren expression
    <?> "expression"

-- | Parse a single variable.
variable :: Parser (Located Expression)
variable = located $ Variable <$> name

-- | Parse a lambda abstraction.
abstraction :: Parser (Located Expression)
abstraction = located do
  punctuation Token.Lambda
  var <- located name
  punctuation Token.Lollipop
  body <- expression
  pure (Abstraction var body)

-- | Parse a function application.
application :: Parser (Located Expression)
application = do
  terms <- some factor
  let firstSpan = locSpan (head terms)
  pure $ foldl1' (foldApp firstSpan) terms
  where
    foldApp spanAcc f x =
      Located
        { locNode = Application f x,
          locSpan = spanAcc <> locSpan x
        }

-- | Parse a literal value.
literal :: Parser (Located Expression)
literal = located $ Literal <$> Literal.literal

-- | Parse a record or a tuple.
tuple :: Parser (Located Expression)
tuple = located $ Tuple <$> conjunction Token.Brace expression

-- | Parse an alternative.
alternative :: Parser (Located Expression)
alternative = located $ Alternative <$> conjunction Token.Angle expression

-- | Parse a pattern match expression
match :: Parser (Located Expression)
match =
  located $
    keyword Token.Match
      >> Match
        <$> expression
        <*> branches
          Token.Brace
          Token.Bar
          pattern'
          expression
