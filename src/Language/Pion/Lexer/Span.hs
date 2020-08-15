-- | Intervals of source positions
module Language.Pion.Lexer.Span (SourceSpan (..), spanned) where

import qualified Text.Megaparsec as Mega

data SourceSpan = SourceSpan
  { spanStart :: !Mega.SourcePos,
    spanStop :: !Mega.SourcePos
  }
  deriving (Eq, Ord, Show)

spanned :: Mega.MonadParsec e s m => m (SourceSpan -> a) -> m a
spanned p = do
  start <- Mega.getSourcePos
  f <- p
  end <- Mega.getSourcePos
  pure $ f (SourceSpan start end)
