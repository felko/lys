-- | Concrete to abstract syntax.
module Language.Pion.Pass.Desugar where

import Control.Monad.Except
import Control.Monad.State
import qualified Language.Pion.Syntax.Abstract as AST
import Language.Pion.Syntax.Common
import qualified Language.Pion.Syntax.Concrete as CST

data DesugarState = DesugarState
  { nameSupply :: !Int
  }
  deriving (Eq, Show)

data DesugarError

type Desugar a = ExceptT DesugarError (State DesugarState) a

desugarExpression :: CST.Expression -> AST.Expression
desugarExpression = \case
  CST.VarE name -> AST.VarE name
  CST.AbsE param body ->
    AST.AbsE
      param
      (desugarExpression <$> body)
  CST.AppE callee argument ->
    AST.AppE
      (desugarExpression <$> callee)
      (desugarExpression <$> argument)
  CST.LitE lit -> AST.LitE lit
  CST.TupleE fields -> AST.TupleE . fmap desugarExpression $ conjunctionToBranches fields
  CST.AltE fields -> AST.AltE . fmap desugarExpression $ conjunctionToBranches fields
  CST.MatchE expr cases ->
    AST.MatchE
      (desugarExpression <$> expr)
      (bimap desugarPattern desugarExpression cases)
  CST.LetE defs expr ->
    AST.LetE
      (bimap desugarPattern desugarExpression defs)
      (desugarExpression <$> expr)

desugarPattern :: CST.Pattern -> AST.Pattern
desugarPattern = \case
  CST.VarP name -> AST.VarP name
  CST.SplitP items -> AST.SplitP (desugarPattern <$> conjunctionToBranches items)
  CST.SelectP label pat -> AST.SelectP label (desugarPattern <$> pat)
  CST.ExtractP pat -> AST.ExtractP (desugarPattern <$> pat)
  CST.UnwrapP pat -> AST.UnwrapP (desugarPattern <$> pat)
  CST.CopyP pats -> AST.CopyP (fmap desugarPattern <$> pats)
