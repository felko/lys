-- | Expression parser.
module Language.Pion.Parser.Expr
  ( expression,
  )
where

import Language.Pion.Lexer.Token
import Language.Pion.Name
import Language.Pion.Parser.Error
import qualified Language.Pion.Parser.Literal as Literal
import Language.Pion.Parser.Monad
import Language.Pion.SourceSpan
import Language.Pion.Syntax.Expr
import Language.Pion.Syntax.Literal

-- | Parse an expression.
expression :: Parser (Located Expr)
expression =
  abstraction
    <|> application
    <?> "expression"

-- | Parse an expression factor in an application.
factor :: Parser (Located Expr)
factor =
  literal
    <|> variable
    <|> between Paren expression
    <?> "expression"

-- | Parse a single variable.
variable :: Parser (Located Expr)
variable = located $ Var . Name <$> identifier

-- | Parse a lambda abstraction.
abstraction :: Parser (Located Expr)
abstraction = located do
  punctuation Lambda
  var <- located $ Name <$> identifier
  punctuation Lollipop
  body <- expression
  pure (Abs var body)

-- | Parse a function application.
application :: Parser (Located Expr)
application = do
  terms <- some factor
  let firstSpan = locSpan (head terms)
  pure $ foldl1' (foldApp firstSpan) terms
  where
    foldApp spanAcc f x =
      Located
        { locNode = App f x,
          locSpan = spanAcc <> locSpan x
        }

-- | Parse a literal value.
literal :: Parser (Located Expr)
literal = located $ Lit <$> Literal.literal
