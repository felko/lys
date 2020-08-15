-- | The parser monad.
module Language.Pion.Parser.Monad
  ( -- * Parser monad
    Parser,
    runParser,

    -- * Parsing primitives
    keyword,
    punctuation,
    connectiveType,
    modalityType,
    unitType,
    identifier,
    integerLiteral,
    floatLiteral,
    charLiteral,
    stringLiteral,

    -- * Parser combinators
    between,
    sepBy,

    -- * Error reporting
    (<?>),

    -- * Source location
    located,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.GADT.Compare
import Data.Semigroup (option)
import Data.Some
import Data.Type.Equality
import Language.Pion.Lexer.Token
import Language.Pion.Parser.Error
import Language.Pion.SourceSpan
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Mega

type Parser = Mega.ParsecT ParseError TokenStream (State Mega.SourcePos)

-- | Run a parser on a token stream, lexed from a source with a given name.
runParser ::
  MonadError ParseErrorRepr m =>
  -- | The 'Parser' action to execute
  Parser a ->
  -- | The name of the input source, useful for error messages
  String ->
  -- | The token stream to parse
  TokenStream ->
  -- | The result, wrapped into an error monad which can throw (pretty printed)
  -- parse errors
  m a
runParser parser name source =
  liftEither
    . first reprParseErrorBundle
    . flip evalState (Mega.initialPos name)
    $ Mega.runParserT parser name source

-- | Consume a single lexeme and return the data associated with the matching
-- token, and fails if the next token represents a different lexeme.
lexeme :: Lexeme a -> Parser a
lexeme expectedLexeme = do
  (tokenData, stop) <- Mega.token matchToken expectedTokens
  option (pure ()) put stop
  pure tokenData
  where
    matchToken (Located (Some Token {..}) span) = do
      Refl <- geq tokenLexeme expectedLexeme
      pure (tokenData, spanStop <$> span)
    expectedTokens = mempty

-- | Attach source information to a parse result.
located :: Parser a -> Parser (Located a)
located p = do
  start <- Mega.getSourcePos
  locNode <- p
  end <- get
  let locSpan = pure $ SourceSpan start end
  pure $ Located {..}

-- | Consume a keyword.
keyword :: Keyword -> Parser ()
keyword = lexeme . Keyword

-- | Consume a punctuation symbol.
punctuation :: Punctuation -> Parser ()
punctuation = lexeme . Punctuation

-- | Consume any simple lexeme (i.e. of type @Lexeme ()@).
anyLexeme :: (Enum a, Bounded a) => (a -> Lexeme ()) -> Parser a
anyLexeme f = Mega.choice $ fmap (\x -> x <$ lexeme (f x)) enumerate

-- | Consume a connective type.
connectiveType :: Parser ConnectiveType
connectiveType = anyLexeme ConnectiveType

-- | Consume a modality type.
modalityType :: Parser ModalityType
modalityType = anyLexeme ModalityType

-- | Consume an unit type.
unitType :: Parser UnitType
unitType = anyLexeme UnitType

-- | Consume a delimiter, identified by its type and whether it is
-- supposed to be an opening or a closing delimiter.
delimiter :: Delimiter -> DelimiterType -> Parser ()
delimiter delim delimType = lexeme (Delimiter delim delimType)

-- | Parse an element surrounded by opening and closing tokens of
-- the same delimiter type.
between :: DelimiterType -> Parser a -> Parser a
between delimType = Mega.between (delimiter Opening delimType) (delimiter Closing delimType)

-- | Consume an identifier.
identifier :: Parser Text
identifier = lexeme Identifier

-- | Consume a decimal integer literal.
integerLiteral :: Parser Integer
integerLiteral = lexeme IntegerLiteral

-- | Consume a floating point literal.
floatLiteral :: Parser Double
floatLiteral = lexeme FloatLiteral

-- | Consume a character literal.
charLiteral :: Parser Char
charLiteral = lexeme CharLiteral

-- | Consume a string literal, which has to fit in one line.
stringLiteral :: Parser Text
stringLiteral = lexeme StringLiteral

-- | @p `sepBy` sep@ parses zero or more occurences of @p@ separated
-- by the punctuation symbol @sep@.
sepBy :: Parser a -> Punctuation -> Parser [a]
sepBy p sep = p `Mega.sepBy` punctuation sep
