{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Type of tokens and lexemes
module Language.Pion.Lexer.Token
  ( DelimiterType (..),
    Delimiter (..),
    Keyword (..),
    PrimType (..),
    Punctuation (..),
    Lexeme (..),
    Token (..),
    TokenStream (..),
  )
where

import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.GADT.Show
import Data.GADT.Show.TH
import Data.List (span)
import Data.Some
import Data.Type.Equality
import GHC.Show
import Language.Pion.Lexer.Span (SourceSpan)
import Language.Pion.Orphans ()
import qualified Text.Megaparsec as Mega
import Prelude hiding (show)

data DelimiterType = Brace | Paren | Brack | Angle
  deriving (Eq, Ord, Show, Enum, Bounded)

data Delimiter = Opening | Closing
  deriving (Eq, Ord, Show, Enum, Bounded)

data Keyword
  = Proc
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
  deriving (Eq, Ord, Show, Enum, Bounded)

data PrimType
  = Tensor
  | Par
  | Plus
  | With
  | WhyNot
  | OfCourse
  | Bottom
  | Top
  | Void
  | End
  deriving (Eq, Ord, Show, Enum, Bounded)

data Punctuation
  = Dot
  | Semicolon
  | Colon
  | Comma
  | DoubleColon
  | Turnstile
  deriving (Eq, Ord, Show, Enum, Bounded)

data Lexeme a where
  -- Keywords
  Keyword :: Keyword -> Lexeme ()
  -- Types
  PrimType :: PrimType -> Lexeme ()
  -- Brackets
  Delimiter :: Delimiter -> DelimiterType -> Lexeme ()
  -- Punctuation
  Punctuation :: Punctuation -> Lexeme ()
  -- Identifier
  Identifier :: Lexeme Text
  -- Literals
  IntegerLiteral :: Lexeme Integer
  FloatLiteral :: Lexeme Double
  CharLiteral :: Lexeme Char
  StringLiteral :: Lexeme Text

deriving instance Eq (Lexeme a)

deriving instance Ord (Lexeme a)

deriving instance Show (Lexeme a)

deriveGEq ''Lexeme
deriveGCompare ''Lexeme
deriveGShow ''Lexeme

data Token a = Token
  { tokenLexeme :: !(Lexeme a),
    tokenData :: !a,
    tokenSpan :: !SourceSpan
  }

instance GEq Token where
  geq (Token l d s) (Token l' d' s')
    | s /= s' = Nothing
    | otherwise = case (l, l') of
      (Keyword kw, Keyword kw') | kw == kw' -> Just Refl
      (PrimType t, PrimType t') | t == t' -> Just Refl
      (Delimiter delimType delim, Delimiter delimType' delim')
        | delimType == delimType', delim == delim' -> Just Refl
      (Punctuation p, Punctuation p') | p == p' -> Just Refl
      (Identifier, Identifier) | d == d' -> Just Refl
      (IntegerLiteral, IntegerLiteral) | d == d' -> Just Refl
      (FloatLiteral, FloatLiteral) | d == d' -> Just Refl
      (CharLiteral, CharLiteral) | d == d' -> Just Refl
      (StringLiteral, StringLiteral) | d == d' -> Just Refl
      _ -> Nothing

instance GCompare Token where
  gcompare (Token l d s) (Token l' d' s') =
    genericCompare s s' . tokenDataOrdering . gcompare l l'
    where
      tokenDataOrdering = case (l, l') of
        (Keyword kw, Keyword kw') -> genericCompare kw kw'
        (PrimType t, PrimType t') -> genericCompare t t'
        (Delimiter delim delimType, Delimiter delim' delimType') ->
          genericCompare delimType delimType' . genericCompare delim delim'
        (Punctuation p, Punctuation p') -> genericCompare p p'
        (Identifier, Identifier) -> genericCompare d d'
        (IntegerLiteral, IntegerLiteral) -> genericCompare d d'
        (FloatLiteral, FloatLiteral) -> genericCompare d d'
        (CharLiteral, CharLiteral) -> genericCompare d d'
        (StringLiteral, StringLiteral) -> genericCompare d d'
        -- If the lexemes are different then the ordering is already covered by
        -- gcompare l l' (see Category instance of GOrdering in Language.Pion.Orphans)
        _ -> GEQ
      genericCompare x y = case compare x y of
        LT -> GLT
        EQ -> GEQ
        GT -> GGT

instance GShow Token where
  gshowsPrec p (Token l d _) = case l of
    Keyword kw ->
      showParen (p > 10) $
        showString "Keyword "
          . showsPrec 11 kw
    PrimType t ->
      showParen (p > 10) $
        showString "PrimType "
          . showsPrec 11 t
    Delimiter delim delimType ->
      showParen (p > 10) $
        showString "Delimiter "
          . showsPrec 11 delim
          . showChar ' '
          . showsPrec 11 delimType
    Punctuation pt ->
      showParen (p > 10) $
        showString "Punctuation "
          . showsPrec 11 pt
    Identifier ->
      showParen (p > 10) $
        showString "Identifier "
          . showsPrec 11 d
    IntegerLiteral ->
      showParen (p > 10) $
        showString "IntegerLiteral "
          . showsPrec 11 d
    FloatLiteral ->
      showParen (p > 10) $
        showString "FloatLiteral "
          . showsPrec 11 d
    CharLiteral ->
      showParen (p > 10) $
        showString "CharLiteral "
          . showsPrec 11 d
    StringLiteral ->
      showParen (p > 10) $
        showString "StringLiteral "
          . showsPrec 11 d

newtype TokenStream = TokenStream
  {getTokens :: [Some Token]}

instance Mega.Stream TokenStream where
  type Token TokenStream = Some Token
  type Tokens TokenStream = [Some Token]

  tokenToChunk :: Proxy TokenStream -> Some Token -> [Some Token]
  tokenToChunk _ = pure

  tokensToChunk :: Proxy TokenStream -> [Some Token] -> [Some Token]
  tokensToChunk _ = id

  chunkToTokens :: Proxy TokenStream -> [Some Token] -> [Some Token]
  chunkToTokens _ = id

  chunkLength :: Proxy TokenStream -> [Some Token] -> Int
  chunkLength _ = length

  chunkEmpty :: Proxy TokenStream -> [Some Token] -> Bool
  chunkEmpty _ = null

  take1_ :: TokenStream -> Maybe (Some Token, TokenStream)
  take1_ (TokenStream tokens) = fmap TokenStream <$> uncons tokens

  takeN_ :: Int -> TokenStream -> Maybe ([Some Token], TokenStream)
  takeN_ n (TokenStream tokens)
    | n <= length tokens = Just $ TokenStream <$> splitAt n tokens
    | otherwise = Nothing

  takeWhile_ :: (Some Token -> Bool) -> TokenStream -> ([Some Token], TokenStream)
  takeWhile_ predicate (TokenStream tokens) = TokenStream <$> span predicate tokens

  showTokens :: Proxy TokenStream -> NonEmpty (Some Token) -> String
  showTokens _ tokens = intercalate ", " . fmap show $ toList tokens

  tokensLength :: Proxy TokenStream -> NonEmpty (Some Token) -> Int
  tokensLength _ = length

  reachOffset :: Int -> Mega.PosState TokenStream -> (String, Mega.PosState TokenStream)
  reachOffset offset Mega.PosState {..} = undefined
