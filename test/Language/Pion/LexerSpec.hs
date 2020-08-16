{-# LANGUAGE TypeApplications #-}

-- | Test suite for the lexer
module Language.Pion.LexerSpec (spec) where

import Data.Char (showLitChar)
import qualified Data.HashSet as HashSet
import Data.Some
import qualified Data.Text.Lazy as LText
import Language.Pion.Lexer.Token
import Language.Pion.Lexer.Tokenize
import Language.Pion.SourceSpan
import Language.Pion.Type
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import qualified Text.Megaparsec as Mega

spec :: Spec
spec = do
  describe "literals" do
    pure ()
  describe "trailing space" do
    it "any lexeme should skip trailing spaces" do
      forAll lexemeWithTrailingSpaceInSource \(l, _, s) ->
        let lexer = (someToken l *> Mega.eof)
            initState = namedInitialState s
            result = Mega.runParser' lexer initState
         in snd result === Right ()

  describe "end of input" do
    it "any lexeme should fail on eof" do
      forAll (elements allLexemes) \l -> do
        Mega.parse (someToken l) sourceName
          `shouldFailOn` ""

  describe "source locations" do
    it "token spans don't include trailing whitespace" do
      forAll lexemeWithTrailingSpaceInSource \(l, s, sTrailing) -> do
        let expectedSpan = mkSpanSameLine 1 (1 + fromIntegral (LText.length s))
        Mega.parse (someToken l) sourceName sTrailing
          `parseSatisfies` \Located {..} ->
            locSpan == pure expectedSpan

  describe "keywords" do
    it "identifier parser rejects reserved keywords" do
      forAll (elements (enumerate @Keyword)) \kw -> do
        Mega.parse (token Identifier) sourceName
          `shouldFailOn` toLazy (keywordText kw)
  where
    sourceName = "(test)"
    namedInitialState source =
      Mega.State
        { stateInput = source,
          stateOffset = 0,
          statePosState =
            Mega.PosState
              { pstateInput = source,
                pstateOffset = 0,
                pstateSourcePos = Mega.initialPos sourceName,
                pstateTabWidth = Mega.defaultTabWidth,
                pstateLinePrefix = ""
              },
          stateParseErrors = []
        }
    mkSpanSameLine :: Int -> Int -> SourceSpan
    mkSpanSameLine colStart colEnd =
      SourceSpan
        { spanStart = Mega.SourcePos sourceName (Mega.mkPos 1) (Mega.mkPos colStart),
          spanEnd = Mega.SourcePos sourceName (Mega.mkPos 1) (Mega.mkPos colEnd)
        }
    lexemeWithTrailingSpaceInSource :: Gen (Some Lexeme, LText, LText)
    lexemeWithTrailingSpaceInSource = do
      (l, s) <- lexemeWithSource
      trailing <- toLText <$> listOf (elements " \t\r\n")
      pure (l, s, s <> trailing)
    lexemeWithSource :: Gen (Some Lexeme, LText)
    lexemeWithSource = do
      l <- elements allLexemes
      s <- sourceFromLexeme l
      pure (l, s)
    sourceFromLexeme :: Some Lexeme -> Gen LText
    sourceFromLexeme = \case
      Some (Keyword kw) -> pure $ toLazy (keywordText kw)
      Some (ConnectiveType c) -> pure $ toLazy (connectiveTypeSymbol c)
      Some (ModalityType m) -> pure $ toLazy (modalityTypeSymbol m)
      Some (UnitType u) -> pure $ toLazy (unitTypeSymbol u)
      Some (Delimiter delim delimType) -> pure $ toLazy (delimiterSymbol delim delimType)
      Some (Punctuation p) -> pure $ toLazy (punctuationSymbol p)
      Some Identifier ->
        fmap toLText (listOf1 (elements (['A' .. 'Z'] ++ ['a' .. 'z'])))
          `suchThat` \ident -> not (HashSet.member (toStrict ident) keywords)
      Some IntegerLiteral -> show <$> chooseInteger (-23454786, 253654768)
      Some FloatLiteral -> show <$> chooseAny @Double
      Some CharLiteral ->
        toLText . concat
          <$> sequence
            [ pure "'",
              flip showLitChar "" <$> chooseAny,
              pure "'"
            ]
      Some StringLiteral ->
        toLText . concat
          <$> sequence
            [ pure "\"",
              fmap concat $ listOf do
                char <- flip showLitChar "" <$> chooseAny @Char
                pure (bool "" char (char == "\"")),
              pure "\""
            ]
