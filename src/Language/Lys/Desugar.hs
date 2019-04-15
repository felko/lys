module Language.Lys.Desugar where

import qualified Language.Lys.Parser.AST as AST
import qualified Language.Lys.Core as Core
import Language.Lys.Types

import Control.Lens

desugar :: AST.Process -> Core.Process
desugar = \case
    AST.NilP -> Core.NilP
    AST.ParP p q -> Core.ParP (desugar p) (desugar q)
    AST.NewP x mt p -> Core.NewP x mt (desugar p)
    AST.OutputP x y p -> Core.OutputP x y (desugar p)
    AST.InputP x y p -> Core.InputP x y (desugar p)
    AST.ReplicateP x y p -> Core.ReplicateP x y (desugar p)
    AST.InjectP x l y p -> Core.InjectP x l y (desugar p)
    AST.MatchP x bs -> Core.MatchP x $ bs <&> branchProcess %~ desugar
    AST.CallP f xs -> Core.CallP f xs
    AST.SourceP x y p -> Core.SourceP x y (desugar p)
    AST.ContractP c x p q -> Core.ContractP c x (desugar p) (desugar q)
