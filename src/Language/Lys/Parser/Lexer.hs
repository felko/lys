{-# LANGUAGE
    OverloadedStrings
  , PatternSynonyms
  #-}

module Language.Lys.Parser.Lexer
    (
    -- * Space handling
      space
    , lexeme
    , symbol

    -- * Megaparsec re-exports
    , Mega.Pos
    , MegaC.char
    , Mega.oneOf
    , Mega.noneOf
    , Mega.notFollowedBy
    , Mega.eof
    , Mega.label
    , (Mega.<?>)
    , Mega.try

    -- * Comments
    , skipLineComment
    , skipBlockComment

    -- * Identifiers
    , identifier
    , upperIdentifier
    , keyword
    , operator

    -- * Literals
    , charLiteral
    , stringLiteral
    , integerLiteral
    , floatLiteral

    -- * Combinators
    , parens, braces, angles
    , commaSep, commaSep1, commaSep2
    , parSep, parSep1, parSep2
    , dotSep1
    ) where

import Language.Lys.Parser.AST
import Language.Lys.Parser.Types
import Language.Lys.Types

import Control.Applicative

import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Text.Megaparsec            as Mega
import qualified Text.Megaparsec.Char       as MegaC
import qualified Text.Megaparsec.Char.Lexer as MegaCL

import Control.Monad.Combinators

-- | Skips all unnecessary characters, such as spaces and comments
space :: Parser ()
space = MegaCL.space MegaC.space1 skipLineComment skipBlockComment

-- | Parses the argument and skips spaces
lexeme :: Parser a -> Parser a
lexeme = MegaCL.lexeme space

-- | Parse a fixed text and skips spaces
symbol :: Text.Text -> Parser Text.Text
symbol = MegaCL.symbol space

-- | Parses a line comment, starts with a double dash
skipLineComment :: Parser ()
skipLineComment = MegaCL.skipLineComment "--"

-- | Parses a line comment, enclosed by "{-" and "-}"
skipBlockComment :: Parser ()
skipBlockComment = MegaCL.skipBlockComment "{-" "-}"

-- | Parses an identifier starting with an uppercase letter
upperIdentifier :: Parser String
upperIdentifier = (:) <$> MegaC.upperChar <*> Mega.many MegaC.alphaNumChar

-- | Parses a reserved keyword
keyword :: String -> Parser ()
keyword w = (lexeme . Mega.try) (MegaC.string (Text.pack w) *> Mega.notFollowedBy MegaC.alphaNumChar)

-- | Set of reserved words
keywords :: Set.Set String
keywords = Set.fromList ["module","import","as","process","type","new","repeat","contract","source","end"]

-- | Parses a process or channel identifier
identifier :: Parser String
identifier = (lexeme . Mega.try) (p >>= check)
    where p = ((:) <$> MegaC.lowerChar <*> Mega.many MegaC.alphaNumChar)
          check x
            | x `Set.member` keywords = fail $ "Keyword " ++ show x ++ " cannot be an identifier"
            | otherwise               = return x

-- | Parses a given operator
operator :: String -> Parser ()
operator sym
    | sym `Set.member` operators = (lexeme . Mega.try) (() <$ MegaC.string (Text.pack sym))
    | otherwise                  = fail $ "Unknown operator " ++ show sym

-- | Set of reserved operators
operators :: Set.Set String
operators = Set.fromList [":","=",".","!","?","|"]

-- | Parses a character literal, delimited by single quotes
charLiteral :: Parser Char
charLiteral = MegaC.char '\'' *> MegaCL.charLiteral <* MegaC.char '\''

-- | Parses a string literal, delimited by double quotes
stringLiteral :: Parser Text.Text
stringLiteral = Text.pack <$> (MegaC.char '"' >> Mega.manyTill p (MegaC.char '"'))
  where p = Mega.label "valid string literal" $ do
                Mega.notFollowedBy (MegaC.char '\n')
                MegaCL.charLiteral

-- | Parses a integer
integerLiteral :: Parser Integer
integerLiteral = MegaCL.decimal

-- | Parses a floating point number
floatLiteral :: Parser Double
floatLiteral = MegaCL.float

-- | Parses the argument wrapped around parenthesis
parens :: Parser a -> Parser a
parens = between (lexeme $ MegaC.char '(') (lexeme $ MegaC.char ')')

-- | Parses the argument wrapped around angles
angles :: Parser a -> Parser a
angles = between (lexeme $ MegaC.char '<') (lexeme $ MegaC.char '>')

-- | Parses the argument wrapped around braces
braces :: Parser a -> Parser a
braces = between (lexeme $ MegaC.char '{') (lexeme $ MegaC.char '}')

-- | Parses zero or more occurences of a parser, separated by commas
commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` symbol ","

-- | Parses one or more occurences of a parser, separated by commas
commaSep1 :: Parser a -> Parser [a]
commaSep1 p = p `sepBy1` symbol ","

-- | Parses two or more occurences of a parser, separated by commas
commaSep2 :: Parser a -> Parser [a]
commaSep2 p = (:) <$> (lexeme p <* symbol ",") <*> commaSep1 p

-- | Parses zero or more occurences of a parser, separated by |
parSep :: Parser a -> Parser [a]
parSep p = p `sepBy` symbol "|"

-- | Parses one or more occurences of a parser, separated by |
parSep1 :: Parser a -> Parser [a]
parSep1 p = p `sepBy1` symbol "|"

-- | Parses two or more occurences of a parser, separated by |
parSep2 :: Parser a -> Parser [a]
parSep2 p = (:) <$> (lexeme p <* symbol "|") <*> parSep1 p

-- | Parses one or more occurences of a parser, separated by .
dotSep1 :: Parser a -> Parser [a]
dotSep1 p = p `sepBy1` symbol "."
