-- | Tokenizer
module Language.Pion.Lexer.Tokenize where

import qualified Data.HashSet as HashSet
import Data.Some
import Language.Pion.Lexer.Error (LexerError (..))
import Language.Pion.Lexer.Span
import Language.Pion.Lexer.Token
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega.Char
import qualified Text.Megaparsec.Char.Lexer as Mega.Lexer

type Lexer a = Mega.Parsec LexerError LText a

lineComment :: Lexer ()
lineComment = Mega.Lexer.skipLineComment "--"

blockComment :: Lexer ()
blockComment = Mega.Lexer.skipBlockCommentNested "{-" "-}"

space :: Lexer ()
space = Mega.Lexer.space Mega.Char.space1 lineComment blockComment

symbol :: Text -> Lexer ()
symbol sym = Mega.Lexer.symbol space (toLazy sym) $> ()

single :: Char -> Lexer ()
single ch = Mega.Lexer.lexeme space (Mega.single ch) $> ()

keywords :: HashSet.HashSet Text
keywords =
  HashSet.fromList
    [ "proc",
      "func",
      "type",
      "module",
      "import",
      "extract",
      "match",
      "join",
      "select",
      "unit",
      "absurd",
      "drop",
      "copy"
    ]

keyword :: Keyword -> Lexer ()
keyword = \case
  Proc -> symbol "proc"
  Func -> symbol "func"
  Type -> symbol "type"
  Module -> symbol "module"
  Import -> symbol "import"
  Extract -> symbol "extract"
  Match -> symbol "match"
  Join -> symbol "join"
  Select -> symbol "select"
  Unit -> symbol "unit"
  Absurd -> symbol "absurd"
  Drop -> symbol "drop"
  Copy -> symbol "copy"

primType :: PrimType -> Lexer ()
primType = \case
  Tensor -> single '⨂'
  Par -> single '⅋'
  Plus -> single '⨁'
  With -> single '&'
  OfCourse -> single '!'
  WhyNot -> single '?'
  Bottom -> single '⊥'
  Top -> single '⊤'
  Void -> symbol "void"
  End -> symbol "end"

delimiter :: Delimiter -> DelimiterType -> Lexer ()
delimiter = curry \case
  (Opening, Brace) -> single '{'
  (Closing, Brace) -> single '}'
  (Opening, Paren) -> single '('
  (Closing, Paren) -> single ')'
  (Opening, Brack) -> single '['
  (Closing, Brack) -> single ']'
  (Opening, Angle) -> single '⟨'
  (Closing, Angle) -> single '⟩'

punctuation :: Punctuation -> Lexer ()
punctuation = \case
  Dot -> single '.'
  Semicolon -> single ';'
  Comma -> single ','
  Colon -> single ':'
  DoubleColon -> single '∷'
  Turnstile -> single '⊢'

lexeme :: Lexeme a -> Lexer a
lexeme l = Mega.Lexer.lexeme space $ case l of
  Keyword kw -> keyword kw
  PrimType t -> primType t
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
  IntegerLiteral -> Mega.Lexer.signed empty Mega.Lexer.decimal
  FloatLiteral -> Mega.Lexer.signed empty Mega.Lexer.float
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

token :: Lexeme a -> Lexer (Token a)
token tokenLexeme = spanned do
  tokenData <- lexeme tokenLexeme
  pure \tokenSpan -> Token {..}

someToken :: Some Lexeme -> Lexer (Some Token)
someToken = traverseSome token

anyToken :: Lexer (Some Token)
anyToken =
  Mega.choice . fmap someToken $
    concat
      [ anyLexeme Keyword,
        anyLexeme PrimType,
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

tokenize :: Lexer TokenStream
tokenize = TokenStream <$> many anyToken
