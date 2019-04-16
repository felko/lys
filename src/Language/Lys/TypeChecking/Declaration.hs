{-# LANGUAGE
    LambdaCase
  , TupleSections
  , BlockArguments
  , OverloadedStrings
  , MultiParamTypeClasses
  #-}

module Language.Lys.TypeChecking.Declaration where
    
import Language.Lys.TypeChecking.Inference
import Language.Lys.TypeChecking.Types
import Language.Lys.Parser.AST
import Language.Lys.Desugar
import Language.Lys.Types.Env
import Language.Lys.Types.Context

import Control.Monad

import Control.Lens hiding (Context)

import qualified Data.Map as Map

instance Checkable Declaration () where
    check = \case
        TypeD n tp t -> do
            typeParams <- checkInfer $ forM tp \case
                ParamTP n -> pure n
                ConstrTP n c -> pure n  -- no kind checking yet
            tau %= introduce n (Scheme typeParams t)
        ProcessD n tp np p -> do
            typeParams <- forM tp \case
                ParamTP n -> pure n
                ConstrTP n c -> pure n  -- no kind checking yet
            ctx <- checkInfer do
                d <- Env . Map.fromList <$> flip traverse np \case
                    InferredNP n -> (n,) <$> freshType "A"
                    AnnotatedNP n t -> pure (n, t)
                fst <$> infer (desugar p :⊢ Context d mempty)
            let params = flip map np \case
                    InferredNP n -> n
                    AnnotatedNP n _ -> n
            gamma %= introduce n (Scheme typeParams (Scheme params ctx))
            
