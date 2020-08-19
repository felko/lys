-- | Expression parser.
module Language.Pion.Parser.Expression
  ( expression,
    variable,
    abstraction,
    application,
    literal,
    tuple,
    alternative,
    match,
    let',
  )
where

import qualified Language.Pion.Lexer.Token as Token
import qualified Language.Pion.Parser.Literal as Literal
import Language.Pion.Parser.Monad
import Language.Pion.Parser.Pattern (pattern', prefixedPattern)
import Language.Pion.SourceSpan
import Language.Pion.Syntax.Concrete.Branch
import Language.Pion.Syntax.Concrete.Expression
import Language.Pion.Syntax.Concrete.Pattern (Pattern)

-- | Parse an expression.
expression :: Parser (Located Expression)
expression =
  match
    <|> let'
    <|> abstraction
    <|> application
    <?> "expression"

-- | Parse an expression factor in an application.
-- A factor can be used in function applications.
factor :: Parser (Located Expression)
factor =
  literal
    <|> try tuple
    <|> alternative
    <|> variable
    <|> between Token.Paren expression
    <?> "expression"

-- | Parse a single variable.
variable :: Parser (Located Expression)
variable = located $ Variable <$> name

-- | Parse a lambda abstraction.
--
-- Example:
--
-- > λ x ⊸ x + x
abstraction :: Parser (Located Expression)
abstraction = located do
  punctuation Token.Lambda
  var <- located name
  punctuation Token.Lollipop
  body <- expression
  pure (Abstraction var body)

-- | Parse a function application.
-- Both the function and the arguments have to be either a literal, a tuple, an
-- alternative, a variable, or a parenthesized expression.
--
-- Example:
--
-- > f x 8 {x : "abc", y : 5} (1 + 3)
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
-- See 'Literal.literal'.
literal :: Parser (Located Expression)
literal = located $ Literal <$> Literal.literal

-- | Parse a record or a tuple.
-- A tuple is either a list of comma-separated expressions or fields,
-- enclosed in parenthesis.
--
-- Examples:
--
--   * Tuple
--
-- > (1, 2, 3)
--
--   * Record
--
-- > (x = 1, y = 2, z = 3)
tuple :: Parser (Located Expression)
tuple =
  located $
    Tuple
      <$> conjunction
        Token.Paren
        Token.Paren
        Token.Equal
        expression

-- | Parse an alternative.
-- An alternative is described by either a list of expressions, or by a list of
-- fields, in both cases enclosed by angle brackets (@⟨@ and @⟩@).
--
-- Examples:
--
--   * Ordered alternative:
--
-- > ⟨1, 2, 3⟩
--
--   * Labelled alternative:
--
-- > ⟨x = 1, y = 2, z = 3⟩
alternative :: Parser (Located Expression)
alternative =
  located $
    Alternative
      <$> conjunction
        Token.Angle
        Token.Angle
        Token.Equal
        expression

-- | Parse a pattern match expression.
--
-- Examples:
--
-- > match n
-- >   { select zero: 0
-- >   | select succ n: n + 1
-- >   }
match :: Parser (Located Expression)
match =
  located $
    keyword Token.Match
      >> Match
        <$> expression
        <*> branches
          Token.Brace
          Token.Bar
          Token.Colon
          pattern'
          expression

-- | Parse let bindings.
-- A single let binding can omit the @let@ keyword when the binding pattern
-- has a prefix (see 'prefixedPattern').
-- Multiple variables can be bound simultaneously by enclosing the definitions
-- in braces, separated by semicolons.
--
-- Examples:
--
--   * Single let binding
--
-- > let x = 123 in x
-- > let select succ nMinusOne = n in nMinusOne
--
--   * Single let bindings with prefixed pattern
--
-- > split {quot, rem} = divMod 1 2 in rem
-- > select succ nMinusOne = n in nMinusOne
--
--   * Multiple let bindings
--
-- > let {
-- >   split (x, y) = f 10;
-- >   z = 2;
-- > } in x + y + z
let' :: Parser (Located Expression)
let' = located do
  bindings <- (pure <$> singleLetBinding) <|> aggregateLetBindings
  keyword Token.In
  body <- expression
  pure $ Let bindings body

-- | Parse a let binding that starts with a given pattern parser.
letBindingWith :: Parser (Located Pattern) -> Parser (Branch Pattern Expression)
letBindingWith patternParser = do
  bound <- patternParser
  punctuation Token.Equal
  value <- expression
  pure (Branch bound value)

-- | Parse a single let binding, whether it is explicit (starts with @let@)
-- or implicit (where the pattern is prefixed by a disambiguating keyword).
singleLetBinding :: Parser (Branch Pattern Expression)
singleLetBinding =
  letBindingWith prefixedPattern
    <|> (keyword Token.Let *> letBindingWith pattern')

-- | Parse many let bindings enclosed in braces and separated by semicolons.
aggregateLetBindings :: Parser (Branches Pattern Expression)
aggregateLetBindings =
  keyword Token.Let
    *> between Token.Brace (letBindingWith pattern' `sepBy` Token.Semicolon)
