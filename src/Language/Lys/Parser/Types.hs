{-# LANGUAGE TemplateHaskell #-}

module Language.Lys.Parser.Types
    ( Parser
    , ParserState(..)
    , rigidTypeVars
    , withParserState
    , isRigid
    ) where

import qualified Text.Megaparsec as Mega

import Control.Monad.State

import Control.Lens

import Data.Text
import Data.Void

import qualified Data.Set as Set

data ParserState = ParserState
    { _rigidTypeVars :: Set.Set String }
makeLenses ''ParserState

type Parser = Mega.ParsecT Void Text (State ParserState)

withParserState :: (ParserState -> ParserState) -> Parser a -> Parser a
withParserState f p = do
    s <- lift get
    modify f *> p <* put s

isRigid :: String -> Parser Bool
isRigid i = Set.member i <$> use rigidTypeVars

