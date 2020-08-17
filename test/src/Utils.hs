-- | Utitliy functions for writing tests.
module Utils
  ( specWithFile,
    testParser,
  )
where

import Control.Exception (bracket)
import Language.Pion.Lexer
import Language.Pion.Parser
import Test.Hspec
import qualified Test.Hspec.Megaparsec as Mega
import qualified Text.Megaparsec as Mega

specWithFile :: FilePath -> String -> (LText -> Expectation) -> Spec
specWithFile path behavior spec = do
  source <- runIO (readFileLText path)
  specify behavior (spec source)

testParser :: Show a => FilePath -> String -> Parser a -> Spec
testParser path behavior parser =
  it (path <> " " <> behavior) do
    source <- readFileLText path
    case runLexer tokenize path source of
      Left lexerError -> fail (Mega.errorBundlePretty lexerError)
      Right stream -> do
        runParser parser path `Mega.shouldSucceedOn` stream
