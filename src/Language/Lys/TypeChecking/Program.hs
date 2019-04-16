{-# LANGUAGE
    LambdaCase
  , TupleSections
  , BlockArguments
  , OverloadedStrings
  , RecordWildCards
  , MultiParamTypeClasses
  #-}

module Language.Lys.TypeChecking.Program where
    
import Language.Lys.TypeChecking.Declaration
import Language.Lys.TypeChecking.Inference
import Language.Lys.TypeChecking.Types
import Language.Lys.Parser.AST
import Language.Lys.Types.Env
import Language.Lys.Types.Context

import Control.Monad

import Control.Lens hiding (Context)

import qualified Data.Map as Map

instance Checkable Program () where
    check Program{..} = mapM_ check _declarations
