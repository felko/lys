-- | Test suite for the parser.
module Language.Pion.ParserSpec (spec) where

import Language.Pion.Parser
import Test.Hspec
import Utils

spec :: Spec
spec = do
  context "Processes" do
    testParser
      "test/passing/test.pion"
      "should parse a module correctly"
      module'
