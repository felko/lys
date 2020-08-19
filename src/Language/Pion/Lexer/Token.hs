{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Type of tokens and lexemes
module Language.Pion.Lexer.Token
  ( -- * Delimiters
    DelimiterType (..),
    Delimiter (..),
    delimiterSymbol,

    -- * Keywords
    Keyword (..),
    keywordText,

    -- * Logical symbols
    ConnectiveType (..),
    ModalityType (..),

    -- * Punctuation
    Punctuation (..),
    punctuationSymbol,

    -- * Abstract token
    Lexeme (..),

    -- * Concrete token
    Token (..),
    LocatedToken,
    SomeLocatedToken,

    -- * Stream of tokens
    Stream (..),
  )
where

import Data.Functor.Classes
import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.GADT.Show
import Data.GADT.Show.TH
import Data.List (span)
import Data.Semigroup (option)
import Data.Some
import qualified Data.Text.Lazy as LText (drop, splitAt, takeWhile)
import Data.Type.Equality
import Language.Pion.Orphans ()
import Language.Pion.Pretty
import Language.Pion.SourceSpan
import Language.Pion.Type
import qualified Text.Megaparsec as Mega
import Text.Show (ShowS, showParen, showString)

-- | Delimiter type.
data DelimiterType
  = -- | @{@ @}@
    Brace
  | -- | @(@ @)@
    Paren
  | -- | @[@ @]@
    Brack
  | -- | @⟨@ @⟩@
    Angle
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Whether a delimiter is an opening or a closing one.
data Delimiter = Opening | Closing
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Symbols of delimiters.
delimiterSymbol :: Delimiter -> DelimiterType -> Text
delimiterSymbol = curry \case
  (Opening, Brace) -> "{"
  (Closing, Brace) -> "}"
  (Opening, Paren) -> "("
  (Closing, Paren) -> ")"
  (Opening, Brack) -> "["
  (Closing, Brack) -> "]"
  (Opening, Angle) -> "⟨"
  (Closing, Angle) -> "⟩"

-- | Type of keywords.
data Keyword
  = Proc
  | Func
  | Type
  | Module
  | Import
  | Extract
  | Unwrap
  | Alternative
  | Fork
  | Match
  | Split
  | Join
  | Select
  | Unit
  | Absurd
  | Drop
  | Copy
  | Run
  | Let
  | In
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Textual content of a keyword.
keywordText :: Keyword -> Text
keywordText = \case
  Proc -> "proc"
  Func -> "func"
  Type -> "type"
  Module -> "module"
  Import -> "import"
  Extract -> "extract"
  Unwrap -> "unwrap"
  Alternative -> "alternative"
  Fork -> "fork"
  Match -> "match"
  Split -> "split"
  Join -> "join"
  Select -> "select"
  Unit -> "unit"
  Absurd -> "absurd"
  Drop -> "drop"
  Copy -> "copy"
  Run -> "run"
  Let -> "let"
  In -> "in"

-- | A punctuation lexeme.
data Punctuation
  = Dot
  | Semicolon
  | Colon
  | Comma
  | DoubleColon
  | Turnstile
  | Lambda
  | Lollipop
  | Bar
  | Equal
  | LeftArrow
  | RightArrow
  deriving (Eq, Ord, Show, Enum, Bounded)

punctuationSymbol :: Punctuation -> Text
punctuationSymbol = \case
  Dot -> "."
  Semicolon -> ";"
  Comma -> ","
  Colon -> ":"
  DoubleColon -> "∷"
  Turnstile -> "⊢"
  Lambda -> "λ"
  Lollipop -> "⊸"
  Bar -> "|"
  Equal -> "="
  LeftArrow -> "←"
  RightArrow -> "→"

-- | Type of lexemes. @Lexeme a@ represents a lexeme
-- which, when tokenized, holds data of type @a@.
data Lexeme a where
  -- Keywords
  Keyword :: Keyword -> Lexeme ()
  -- Types
  ConnectiveType :: ConnectiveType -> Lexeme ()
  UnitType :: ConnectiveType -> Lexeme ()
  ModalityType :: ModalityType -> Lexeme ()
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

instance Eq1 Lexeme where
  liftEq _ = defaultEq

deriveGEq ''Lexeme
deriveGCompare ''Lexeme
deriveGShow ''Lexeme

-- | A concrete token.
data Token a = Token
  { -- | The lexeme of this token
    tokenLexeme :: !(Lexeme a),
    -- | The length ofthe token, in characters
    tokenLength :: Int,
    -- | The data associated with the token, depending on its lexeme
    tokenData :: !a
  }

instance Eq1 Token where
  liftEq eq (Token l o d) (Token l' o' d') =
    (l `defaultEq` l') && (o == o') && (d `eq` d')

instance Show1 Token where
  liftShowsPrec _ _ = gshowsPrec

instance GEq Token where
  geq (Token l o d) (Token l' o' d')
    | o == o' = case (l, l') of
      (Keyword kw, Keyword kw') | kw == kw' -> Just Refl
      (ConnectiveType c, ConnectiveType c') | c == c' -> Just Refl
      (UnitType c, UnitType c') | c == c' -> Just Refl
      (ModalityType m, ModalityType m') | m == m' -> Just Refl
      (Delimiter delimType delim, Delimiter delimType' delim')
        | delimType == delimType', delim == delim' -> Just Refl
      (Punctuation p, Punctuation p') | p == p' -> Just Refl
      (Identifier, Identifier) | d == d' -> Just Refl
      (IntegerLiteral, IntegerLiteral) | d == d' -> Just Refl
      (FloatLiteral, FloatLiteral) | d == d' -> Just Refl
      (CharLiteral, CharLiteral) | d == d' -> Just Refl
      (StringLiteral, StringLiteral) | d == d' -> Just Refl
      _ -> Nothing
    | otherwise = Nothing

instance GCompare Token where
  gcompare (Token l o d) (Token l' o' d') =
    genericCompare o o' . tokenDataOrdering . gcompare l l'
    where
      tokenDataOrdering = case (l, l') of
        (Keyword kw, Keyword kw') -> genericCompare kw kw'
        (ConnectiveType c, ConnectiveType c') -> genericCompare c c'
        (ModalityType m, ModalityType m') -> genericCompare m m'
        (UnitType u, UnitType u') -> genericCompare u u'
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
  gshowsPrec p Token {..} = case tokenLexeme of
    Keyword {} -> showTokenWith ()
    ConnectiveType {} -> showTokenWith ()
    ModalityType {} -> showTokenWith ()
    UnitType {} -> showTokenWith ()
    Delimiter {} -> showTokenWith ()
    Punctuation {} -> showTokenWith ()
    Identifier -> showTokenWith tokenData
    IntegerLiteral -> showTokenWith tokenData
    FloatLiteral -> showTokenWith tokenData
    CharLiteral -> showTokenWith tokenData
    StringLiteral -> showTokenWith tokenData
    where
      showFields [] = const ""
      showFields [(field, value)] =
        showString (field ++ " = " ++ value)
      showFields ((field, value) : fields) =
        showString (field ++ " = " ++ value ++ ", ")
          . showFields fields
      showTokenWith :: forall a. Show a => a -> ShowS
      showTokenWith value =
        showParen (p > 11) $
          showString "Token "
            . showString "{ "
            . showFields
              [ ("tokenLexeme", show tokenLexeme),
                ("tokenLength", show tokenLength),
                ("tokenData", show value)
              ]
            . showString " }"

instance Pretty (Token a) where
  pretty :: forall ann. Token a -> Doc ann
  pretty Token {..} = case tokenLexeme of
    Keyword kw -> pretty (keywordText kw)
    ConnectiveType c -> pretty (connectiveTypeSymbol c)
    ModalityType m -> pretty (modalityTypeSymbol m)
    UnitType u -> pretty (unitTypeSymbol u)
    Delimiter delim delimType -> pretty (delimiterSymbol delim delimType)
    Punctuation p -> pretty (punctuationSymbol p)
    Identifier -> pretty tokenData
    IntegerLiteral -> pretty tokenData
    FloatLiteral -> pretty tokenData
    CharLiteral -> prettyCharLiteral tokenData
    StringLiteral -> prettyStringLiteral tokenData

-- | A stream of tokens
data Stream = Stream
  { -- | The source associated with the stream. This allows
    -- the parse error to report the offending lines
    streamSource :: LText,
    -- | The list of tokens resulting from the lexing.
    streamTokens :: [SomeLocatedToken]
  }

-- | A concrete token together with information
-- about its location in the source file.
type LocatedToken = Compose Located Token

-- | Existential version of 'LocatedToken'.
-- Note that the @Some@ wraps around the token instead of @Some (Compose Located Token)@.
-- This avoids pattern matching on @Some@ if we only want to retrieve the source position.
type SomeLocatedToken = Located (Some Token)

-- | Helper function for the @Stream@ instance.
someTokenLength :: SomeLocatedToken -> Int
someTokenLength Located {..} = case locNode of
  Some Token {..} -> tokenLength

-- Adapted from <https://markkarpov.com/tutorial/megaparsec.html#working-with-custom-input-streams>
-- to work on lazy text.
instance Mega.Stream Stream where
  type Token Stream = SomeLocatedToken
  type Tokens Stream = [SomeLocatedToken]

  tokenToChunk :: Proxy Stream -> SomeLocatedToken -> [SomeLocatedToken]
  tokenToChunk _ = pure

  tokensToChunk :: Proxy Stream -> [SomeLocatedToken] -> [SomeLocatedToken]
  tokensToChunk _ = id

  chunkToTokens :: Proxy Stream -> [SomeLocatedToken] -> [SomeLocatedToken]
  chunkToTokens _ = id

  chunkLength :: Proxy Stream -> [SomeLocatedToken] -> Int
  chunkLength _ = length

  chunkEmpty :: Proxy Stream -> [SomeLocatedToken] -> Bool
  chunkEmpty _ = null

  take1_ (Stream source tokens) = case tokens of
    [] -> Nothing
    token : remaining ->
      let remainingSource = LText.drop (fromIntegral $ someTokenLength token) source
       in Just (token, Stream remainingSource remaining)

  takeN_ :: Int -> Stream -> Maybe ([SomeLocatedToken], Stream)
  takeN_ n (Stream source tokens)
    | n <= 0 = Just ([], Stream source tokens)
    | null tokens = Nothing
    | otherwise =
      let (consumed, remaining) = splitAt n tokens
       in case nonEmpty consumed of
            Nothing -> Just (consumed, Stream source remaining)
            Just nonEmptyConsumed ->
              let consumedLength = Mega.tokensLength (Proxy @Stream) nonEmptyConsumed
               in Just (consumed, Stream (LText.drop (fromIntegral consumedLength) source) remaining)

  takeWhile_ :: (SomeLocatedToken -> Bool) -> Stream -> ([SomeLocatedToken], Stream)
  takeWhile_ predicate (Stream source tokens) =
    let (consumed, remaining) = span predicate tokens
     in case nonEmpty consumed of
          Nothing -> (consumed, Stream source remaining)
          Just nonEmptyConsumed ->
            let consumedLength = Mega.tokensLength (Proxy @Stream) nonEmptyConsumed
             in (consumed, Stream (LText.drop (fromIntegral consumedLength) source) remaining)

  showTokens :: Proxy Stream -> NonEmpty SomeLocatedToken -> String
  showTokens _ tokens = docToString . hsep . fmap pretty $ toList tokens

  tokensLength :: Proxy Stream -> NonEmpty SomeLocatedToken -> Int
  tokensLength _ = sum . fmap someTokenLength

  reachOffset :: Int -> Mega.PosState Stream -> (String, Mega.PosState Stream)
  reachOffset offset Mega.PosState {..} =
    ( prefix <> restOfLine,
      Mega.PosState
        { pstateInput = Stream postSource postTokens,
          pstateOffset = max pstateOffset offset,
          pstateSourcePos = newSourcePos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix = prefix
        }
    )
    where
      sameLine = Mega.sourceLine newSourcePos == Mega.sourceLine pstateSourcePos
      (preTokens, postTokens) = splitAt (offset - pstateOffset) (streamTokens pstateInput)
      (preSource, postSource) = LText.splitAt (fromIntegral tokensConsumed) (streamSource pstateInput)
      newSourcePos =
        case postTokens of
          [] -> pstateSourcePos
          (token : _) -> option pstateSourcePos spanStart (locSpan token)
      tokensConsumed =
        case nonEmpty preTokens of
          Nothing -> 0
          Just nonEmptyPreTokens -> Mega.tokensLength (Proxy @Stream) nonEmptyPreTokens
      restOfLine = toString (LText.takeWhile (/= '\n') postSource)
      prefix =
        if sameLine
          then pstateLinePrefix <> toString preSource
          else toString preSource

instance Pretty Stream where
  pretty :: forall ann. Stream -> Doc ann
  pretty Stream {..} = hsep $ prettyToken <$> streamTokens
    where
      prettyToken :: SomeLocatedToken -> Doc ann
      prettyToken Located {..} = foldSome pretty locNode
