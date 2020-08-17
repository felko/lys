-- | Tokenizer.
module Language.Pion.Lexer.Tokenize
  ( keywords,
    keyword,
    delimiter,
    punctuation,
    lexeme,
    allLexemes,
    token,
    someToken,
    anyToken,
    tokenize,
  )
where

import qualified Data.HashSet as HashSet
import Data.Some
import Language.Pion.Lexer.Error (LexerError (..))
import Language.Pion.Lexer.Monad
import Language.Pion.Lexer.Token
import Language.Pion.SourceSpan
import Language.Pion.Type
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega.Char
import qualified Text.Megaparsec.Char.Lexer as Mega.Lexer

-- | Set of keywords. Used for validating identifiers.
keywords :: HashSet.HashSet Text
keywords = HashSet.fromList $ (keywordText <$> enumerate) ++ ["void", "end"]

-- | Consume a keyword.
keyword :: Keyword -> Lexer ()
keyword = symbol . keywordText

-- | Consume a delimiter.
delimiter :: Delimiter -> DelimiterType -> Lexer ()
delimiter delim delimType = symbol (delimiterSymbol delim delimType)

-- | Consume a punctuation symbol.
punctuation :: Punctuation -> Lexer ()
punctuation = symbol . punctuationSymbol

-- | Consume a lexeme and return the corresponding parsed data.
lexeme :: Lexeme a -> Lexer a
lexeme l = case l of
  Keyword kw -> keyword kw
  ConnectiveType c -> symbol (connectiveTypeSymbol c)
  ModalityType m -> symbol (modalityTypeSymbol m)
  UnitType u -> symbol (unitTypeSymbol u)
  Delimiter delim delimType -> delimiter delim delimType
  Punctuation p -> punctuation p
  Identifier -> do
    ident <-
      fmap toText $
        (:)
          <$> Mega.Char.letterChar
          <*> many (Mega.Char.alphaNumChar)
    guard (not $ HashSet.member ident keywords)
      <|> Mega.customFailure (InvalidIdentifier ident)
    pure ident
  IntegerLiteral -> Mega.Lexer.signed space Mega.Lexer.decimal
  FloatLiteral -> Mega.Lexer.signed space Mega.Lexer.float
  CharLiteral ->
    Mega.between
      (Mega.Char.char '\'')
      (Mega.Char.char '\'')
      Mega.Lexer.charLiteral
      <|> Mega.customFailure MalformedCharLiteral
  StringLiteral ->
    fmap toText $
      Mega.Char.char '"'
        *> Mega.manyTill Mega.Lexer.charLiteral (Mega.Char.char '"')
        <|> Mega.customFailure MalformedStringLiteral

-- | Consume a lexeme together with source location information.
token :: Lexeme a -> Lexer (LocatedToken a)
token tokenLexeme = do
  startOffset <- getOffset
  Located {locNode = tokenData, locSpan} <-
    skipTrailingSpaces $
      located (lexeme tokenLexeme)
  endOffset <- getOffset
  let tokenLength = endOffset - startOffset
  pure . Compose $ Located {locNode = Token {..}, locSpan}
  where
    getOffset = Mega.stateOffset <$> Mega.getParserState

-- | Consume an existentially quantified token, with location information.
someToken :: Some Lexeme -> Lexer SomeLocatedToken
someToken = getCompose . traverseSome (Compose . fmap getCompose . token)

-- | Enumerate all possible lexemes.
allLexemes :: [Some Lexeme]
allLexemes =
  concat
    [ anyLexeme Keyword,
      anyLexeme ConnectiveType,
      anyLexeme ModalityType,
      anyLexeme UnitType,
      anyLexeme2 Delimiter,
      anyLexeme Punctuation,
      [ Some Identifier,
        Some IntegerLiteral,
        Some FloatLiteral,
        Some CharLiteral,
        Some StringLiteral
      ]
    ]
  where
    anyLexeme f = Some . f <$> enumerate
    anyLexeme2 f = (Some .) . f <$> enumerate <*> enumerate

-- | Consume any valid token.
anyToken :: Lexer SomeLocatedToken
anyToken = Mega.choice (someToken <$> allLexemes)

-- | Tokenize the input source into a stream.
tokenize :: Lexer Stream
tokenize = do
  source <- Mega.stateInput <$> Mega.getParserState
  Stream source <$> many anyToken <* Mega.eof
