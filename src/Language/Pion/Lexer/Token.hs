-- | Type of tokens
module Language.Pion.Lexer.Token (Token (..)) where

data Token
  = -- Keywords
    Proc
  | Func
  | Type
  | Module
  | Import
  | Extract
  | Match
  | Join
  | Select
  | Unit
  | Absurd
  | Drop
  | Copy
  | -- Types
    Tensor
  | Par
  | Plus
  | With
  | WhyNot
  | OfCourse
  | Bottom
  | Top
  | Zero
  | One
  | -- Brackets
    LeftBrace
  | RightBrace
  | LeftParen
  | RightParen
  | LeftAngle
  | RightAngle
  | LeftBrack
  | RightBrack
  | -- Punctuation
    Dot
  | Semicolon
  | Colon
  | Comma
  | DoubleColon
  | Turnstile
  | -- Identifier
    Identifier Text
  | -- Literals
    IntegerLiteral Integer
  | FloatLiteral Double
  | CharLiteral Char
  | StringLiteral Text
  deriving (Eq, Ord, Show)
