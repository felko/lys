-- | Parser
module Language.Pion.Parser
  ( parseExpression,
  )
where

import Control.Monad.Except (MonadError)
import Language.Pion.Lexer.Token (TokenStream)
import Language.Pion.Parser.Error (ParseErrorRepr)
import Language.Pion.Parser.Expr (expression)
import Language.Pion.Parser.Monad (runParser)
import Language.Pion.SourceSpan (Located)
import Language.Pion.Syntax.Expr (Expr)

parseExpression :: MonadError ParseErrorRepr m => String -> TokenStream -> m (Located Expr)
parseExpression = runParser expression
