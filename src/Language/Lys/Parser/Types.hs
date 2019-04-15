module Language.Lys.Parser.Types
    ( Parser
    ) where

import qualified Text.Megaparsec as Mega

import Data.Text
import Data.Void

type Parser = Mega.Parsec Void Text
