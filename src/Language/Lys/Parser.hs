module Language.Lys.Parser
    ( module Language.Lys.Parser.Types
    , module Language.Lys.Parser.AST
    , program
    , declaration
    , process
    , parse
    , parseFile
    ) where

import Language.Lys.Parser.AST
import Language.Lys.Parser.Program     (program)
import Language.Lys.Parser.Declaration (declaration)
import Language.Lys.Parser.Process     (process)
import Language.Lys.Parser.Name        (name)
import Language.Lys.Parser.Type        (type')
import Language.Lys.Parser.Types

import Control.Monad.State

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import qualified Text.Megaparsec as Mega

defaultParserState :: ParserState
defaultParserState = ParserState
    { _rigidTypeVars = mempty }

parse :: Parser a -> String -> Text.Text -> Either String a
parse parser name src = case evalState (Mega.runParserT parser name src) defaultParserState of
    Left bundle -> Left (Mega.errorBundlePretty bundle)
    Right val -> Right val

parseFile :: FilePath -> IO (Either String Program)
parseFile path = parse program path <$> Text.readFile path
