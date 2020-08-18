-- | Pattern parser.
module Language.Pion.Parser.Pattern
  ( pattern',
    prefixedPattern,
    variable,
    split,
    select,
    extract,
    unwrap,
    copy,
  )
where

import qualified Language.Pion.Lexer.Token as Token
import Language.Pion.Parser.Monad
import Language.Pion.SourceSpan
import Language.Pion.Syntax.Pattern
import Prelude hiding (drop)

-- | Parse a pattern.
pattern' :: Parser (Located Pattern)
pattern' =
  prefixedPattern
    <|> located variable
    <?> "pattern"

-- | Parse a pattern that has a prefixing keyword.
prefixedPattern :: Parser (Located Pattern)
prefixedPattern =
  located
    ( split
        <|> select
        <|> extract
        <|> unwrap
        <|> copy
        <?> "prefixed pattern"
    )

-- | Parse a variable pattern.
-- The variable name must be a valid identifier.
variable :: Parser Pattern
variable = Variable <$> name

-- | Parse a split pattern.
--
-- Example:
--
--  * Splitting on a tuple:
--
-- > split (x, y, z)
--
--  * Splitting on a record:
--
-- > split (x = a, y = b, z = c)
split :: Parser Pattern
split =
  keyword Token.Split
    >> Split <$> conjunction Token.Paren Token.Paren Token.Equal pattern'

-- | Parse a selector pattern.
--
-- Example:
--
-- > select succ n
select :: Parser Pattern
select =
  keyword Token.Select
    >> Select <$> located label <*> pattern'

-- | Parse an extract pattern.
--
-- Example:
--
-- > extract x
extract :: Parser Pattern
extract =
  keyword Token.Extract
    >> Extract <$> pattern'

-- | Parse an unwrap pattern.
--
-- Example:
--
-- > unwrap x
unwrap :: Parser Pattern
unwrap =
  keyword Token.Unwrap
    >> Unwrap <$> pattern'

-- | Parse a copy pattern.
--
-- Example:
--
-- copy (x, y, z)
copy :: Parser Pattern
copy =
  keyword Token.Copy
    >> Copy <$> between Token.Paren (pattern' `sepBy` Token.Comma)
