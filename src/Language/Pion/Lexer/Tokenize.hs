-- | Tokenizer
module Language.Pion.Lexer.Tokenize where

import qualified Data.HashSet as HashSet
import Language.Pion.Lexer.Error (TokenError (..))
import Language.Pion.Lexer.Token
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega.Char
import qualified Text.Megaparsec.Char.Lexer as Mega.Lexer

type Lexer a = Mega.Parsec TokenError LText a

lineComment :: Lexer ()
lineComment = Mega.Lexer.skipLineComment "--"

blockComment :: Lexer ()
blockComment = Mega.Lexer.skipBlockCommentNested "{-" "-}"

space :: Lexer ()
space = Mega.Lexer.space Mega.Char.space1 lineComment blockComment

lexeme :: Lexer a -> Lexer a
lexeme = Mega.Lexer.lexeme space

symbol :: Text -> Lexer ()
symbol sym = Mega.Lexer.symbol space (toLazy sym) $> ()

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

exactTokenWith :: (t -> Lexer x) -> t -> Token -> Lexer Token
exactTokenWith lex x token = lexeme $ lex x $> token

exactToken :: Text -> Token -> Lexer Token
exactToken = exactTokenWith (Mega.chunk . toLazy)

exactCharToken :: Char -> Token -> Lexer Token
exactCharToken = exactTokenWith Mega.single

identifier :: Lexer Text
identifier = lexeme do
  ident <-
    fmap toText $
      (:)
        <$> Mega.Char.letterChar
        <*> many (Mega.Char.alphaNumChar)
  guard (not $ HashSet.member ident keywords)
    <|> Mega.customFailure (InvalidIdentifier ident)
  pure ident

integerLiteral :: Lexer Integer
integerLiteral = Mega.Lexer.signed empty Mega.Lexer.decimal

floatLiteral :: Lexer Double
floatLiteral = Mega.Lexer.signed empty Mega.Lexer.float

charLiteral :: Lexer Char
charLiteral =
  lexeme $
    Mega.between
      (Mega.Char.char '\'')
      (Mega.Char.char '\'')
      Mega.Lexer.charLiteral
      <|> Mega.customFailure MalformedCharLiteral

stringLiteral :: Lexer Text
stringLiteral =
  lexeme . fmap toText $
    Mega.Char.char '"'
      *> Mega.manyTill Mega.Lexer.charLiteral (Mega.Char.char '"')
      <|> Mega.customFailure MalformedStringLiteral

tokenize :: Lexer [Token]
tokenize = many token
  where
    token =
      keywordToken
        <|> punctuationToken
        <|> identifierToken
        <|> literalToken
        <|> Mega.customFailure UnknownToken
    keywordToken =
      Mega.choice $
        uncurry exactToken
          <$> [ ("proc", Proc),
                ("func", Func),
                ("type", Type),
                ("module", Module),
                ("import", Import),
                ("extract", Extract),
                ("match", Match),
                ("join", Join),
                ("select", Select),
                ("unit", Unit),
                ("absurd", Absurd),
                ("drop", Drop),
                ("copy", Copy)
              ]
    punctuationToken =
      Mega.choice $
        uncurry exactCharToken
          <$> [ ('{', LeftBrace),
                ('}', RightBrace),
                ('(', LeftParen),
                (')', RightParen),
                ('[', LeftBrack),
                (']', RightBrack),
                ('⟨', LeftAngle),
                ('⟩', LeftAngle),
                ('.', Dot),
                (';', Semicolon),
                (':', Colon),
                (',', Comma),
                ('∷', DoubleColon),
                ('⊢', Turnstile),
                ('⨂', Tensor),
                ('⅋', Par),
                ('⨁', Plus),
                ('&', With),
                ('!', OfCourse),
                ('?', WhyNot),
                ('⊥', Bottom),
                ('⊤', Top),
                ('0', Zero),
                ('1', One)
              ]
    identifierToken = Identifier <$> identifier
    literalToken =
      Mega.choice
        [ IntegerLiteral <$> integerLiteral,
          FloatLiteral <$> floatLiteral,
          CharLiteral <$> charLiteral,
          StringLiteral <$> stringLiteral
        ]
