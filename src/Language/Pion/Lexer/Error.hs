-- | Lexer errors
module Language.Pion.Lexer.Error (LexerError(..),
                                  TokenError(..), tokenErrorBundleToLexerError) where

import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle(..))
import qualified Data.Set as Set

import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Error as Mega

import qualified Data.Text.Lazy as LText

newtype LexerError = LexerError { getLexerError :: Doc AnsiStyle }

data TokenError
  = InvalidIdentifier Text
  | MalformedCharLiteral
  | MalformedStringLiteral
  | UnknownToken
  deriving (Eq, Ord, Show)

instance Mega.ShowErrorComponent TokenError where
  showErrorComponent = \case
    InvalidIdentifier t -> "Invalid identifier: `" <> toString t <> "' is a keyword"
    MalformedCharLiteral -> "Malformed character literal"
    MalformedStringLiteral -> "Malformed string literal"
    UnknownToken -> "Lexer error"

-- | An adaptation of <https://hackage.haskell.org/package/megaparsec-8.0.0/docs/src/Text.Megaparsec.Error.html#errorBundlePretty>
-- to handle ANSI-annotated docs.
tokenErrorBundleToLexerError
  :: Mega.ParseErrorBundle LText TokenError -- ^ Parse error bundle to display
  -> LexerError                             -- ^ Textual rendition of the bundle
tokenErrorBundleToLexerError Mega.ParseErrorBundle {..} =
  let (r, _) = foldl' f (id, bundlePosState) bundleErrors
  in LexerError (r "")
  where
    errorFancyLength :: Mega.ErrorFancy TokenError -> Int
    errorFancyLength = \case
      Mega.ErrorCustom a -> Mega.errorComponentLen a
      _             -> 1
    errorItemLength :: Mega.ErrorItem Char -> Int
    errorItemLength = \case
      Mega.Tokens ts -> Mega.tokensLength (Proxy @LText) ts
      _         -> 1
    f :: (Doc AnsiStyle -> Doc AnsiStyle, Mega.PosState LText)
      -> Mega.ParseError LText TokenError
      -> (Doc AnsiStyle -> Doc AnsiStyle, Mega.PosState LText)
    f (o, !pst) e = (o . (outChunk <>), pst')
      where
        (sline, pst') = Mega.reachOffset (Mega.errorOffset e) pst
        epos = Mega.pstateSourcePos pst'
        outChunk =
          "\n" <> pretty (Mega.sourcePosPretty epos) <> ":\n" <>
          padding <> "|\n" <>
          fromString lineNumber <+> " | " <> fromString sline <> "\n" <>
          padding <> "| " <> rpadding <> pointer <> "\n" <>
          fromString (Mega.parseErrorTextPretty e)
        lineNumber = show . Mega.unPos $ Mega.sourceLine epos
        padding = fromString $ replicate (length lineNumber + 1) ' '
        rpadding =
          if pointerLen > 0
            then fold $ replicate rpshift " "
            else ""
        rpshift = Mega.unPos (Mega.sourceColumn epos) - 1
        pointer = fold $ replicate pointerLen "^"
        pointerLen =
          if rpshift + elen > slineLen
            then slineLen - rpshift + 1
            else elen
        slineLen = length sline
        elen =
          case e of
            Mega.TrivialError _ Nothing _ -> 1
            Mega.TrivialError _ (Just x) _ -> errorItemLength x
            Mega.FancyError _ xs ->
              Set.foldl' (\a b -> max a (errorFancyLength b)) 1 xs
