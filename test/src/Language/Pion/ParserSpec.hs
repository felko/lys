-- | Test suite for the parser.
module Language.Pion.ParserSpec (spec) where

import Language.Pion.Lexer (lex)
import Language.Pion.Parser
import Test.Hspec
import qualified Test.Hspec.Megaparsec as Mega
import Utils

spec :: Spec
spec = do
  context "Processes" do
    testParser
      "test/passing/test.pion"
      "should parse a process correctly"
      process
