-- | Pattern parser.
module Language.Pion.Parser.Pattern
  ( pattern',
  )
where

import qualified Language.Pion.Lexer.Token as Token
import Language.Pion.Name
import Language.Pion.Parser.Error
import Language.Pion.Parser.Monad
import Language.Pion.SourceSpan
import Language.Pion.Syntax.Pattern
import Prelude hiding (drop)

pattern' :: Parser (Located Pattern)
pattern' =
  located
    ( split
        <|> select
        <|> extract
        <|> unwrap
        <|> copy
        <|> variable
        <?> "pattern"
    )

variable :: Parser Pattern
variable = Variable <$> name

split :: Parser Pattern
split =
  keyword Token.Split
    >> Split <$> conjunction Token.Brace pattern'

select :: Parser Pattern
select =
  keyword Token.Select
    >> Select <$> located label <*> pattern'

extract :: Parser Pattern
extract =
  keyword Token.Extract
    >> Extract <$> pattern'

unwrap :: Parser Pattern
unwrap =
  keyword Token.Unwrap
    >> Unwrap <$> pattern'

copy :: Parser Pattern
copy =
  keyword Token.Copy
    >> Copy <$> between Token.Brace (pattern' `sepBy` Token.Comma)
