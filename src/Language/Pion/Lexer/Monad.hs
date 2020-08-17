-- | Lexer monad.
module Language.Pion.Lexer.Monad
  ( -- * Lexer monad
    Lexer,
    runLexer,

    -- * Whitespace
    lineComment,
    blockComment,
    space,
    skipTrailingSpaces,

    -- * Lexing primitives
    symbol,
    single,

    -- * Source location
    located,
  )
where

import Control.Monad.Except
import Language.Pion.Lexer.Error
import Language.Pion.SourceSpan
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega.Char
import qualified Text.Megaparsec.Char.Lexer as Mega.Lexer

-- | The lexer monad.
-- Represented as a parser on lazy text which can throw lexer errors.
type Lexer a = Mega.Parsec LexerError LText a

-- | Run a lexer on source code with a given name.
runLexer ::
  MonadError LexerErrorBundle m =>
  -- | The 'Lexer' action to execute
  Lexer a ->
  -- | The name of the input source, useful for error messages
  String ->
  -- | The source code to parse
  LText ->
  -- | The result, wrapped into an error monad which can throw (pretty printed)
  -- lexer errors
  m a
runLexer lexer name source =
  liftEither $ Mega.runParser lexer name source

-- | Skip a line comment, which is prefixed by @--@.
lineComment :: Lexer ()
lineComment = Mega.Lexer.skipLineComment "--"

-- | Skip a block comment, delimited by @{-@ and @-}@.
blockComment :: Lexer ()
blockComment = Mega.Lexer.skipBlockCommentNested "{-" "-}"

-- | Consume a space character.
space :: Lexer ()
space = Mega.Lexer.space Mega.Char.space1 lineComment blockComment

-- | Consume a given 'Lexer' action as well as skip trailing whitespace.
skipTrailingSpaces :: Lexer a -> Lexer a
skipTrailingSpaces = Mega.Lexer.lexeme space

-- | Consume the given prefix and skip trailing whitespace.
symbol :: Text -> Lexer ()
symbol sym = Mega.chunk (toLazy sym) $> ()

-- | Consume a single character and skip trailing whitespace.
single :: Char -> Lexer ()
single ch = Mega.single ch $> ()

-- | Attach source information to a lexer result.
located :: Lexer a -> Lexer (Located a)
located p = do
  start <- Mega.getSourcePos
  locNode <- p
  end <- Mega.getSourcePos
  let locSpan = pure $ SourceSpan start end
  pure $ Located {..}
