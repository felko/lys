-- | Parser.
module Language.Pion.Parser
  ( parseExpression,
  )
where

import Control.Monad.Except (MonadError)
import Language.Pion.Lexer.Token (Stream)
import Language.Pion.Parser.Error (ParseErrorRepr)
import Language.Pion.Parser.Expression (expression)
import Language.Pion.Parser.Monad (runParser)
import Language.Pion.SourceSpan (Located)
import Language.Pion.Syntax.Expression (Expression)

parseExpression :: MonadError ParseErrorRepr m => String -> Stream -> m (Located Expression)
parseExpression = runParser expression
