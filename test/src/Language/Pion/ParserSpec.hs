-- | Test suite for the parser.
module Language.Pion.ParserSpec (spec) where

import qualified Language.Pion.Parser as Parser
import Test.Hspec
import Utils

spec :: Spec
spec = do
  context "Processes" do
    testParser
      "test/passing/test.pion"
      "should parse a module correctly"
      Parser.module'
