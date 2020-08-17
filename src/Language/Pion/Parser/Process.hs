-- | Process parser.
module Language.Pion.Parser.Process
  ( process,
  )
where

import qualified Language.Pion.Lexer.Token as Token
import Language.Pion.Name
import Language.Pion.Parser.Expression
import Language.Pion.Parser.Monad
import Language.Pion.Parser.Pattern
import Language.Pion.SourceSpan
import Language.Pion.Syntax.Branch
import Language.Pion.Syntax.Process
import qualified Text.Megaparsec as Mega
import Prelude hiding (join)

-- | Parse an process.
process :: Parser (Located Process)
process = located $ Process <$> action `sepBy` Token.Semicolon

action :: Parser (Located Action)
action =
  located $
    ( Mega.try run
        <|> Mega.try preAction
        <|> Mega.try postAction
        <|> join
        <|> fork
        <|> alternative
        <|> match
        <?> "action"
    )

preAction :: Parser Action
preAction = do
  n <- located name
  punctuation Token.RightArrow
  p <- pattern'
  pure $ Pre n p

postAction :: Parser Action
postAction = do
  n <- located name
  punctuation Token.LeftArrow
  e <- expression
  pure $ Post n e

run :: Parser Action
run = do
  keyword Token.Run
  procName <- located identifier
  ports <- portMap
  pure (Run procName ports)

branchingAction ::
  (Located Name -> Branches c Process -> Action) ->
  Token.Keyword ->
  Parser c ->
  Parser Action
branchingAction constr prefix caseParser =
  keyword prefix
    >> constr
    <$> located name
    <*> branches
      Token.Brace
      Token.Bar
      (located caseParser)
      process

join :: Parser Action
join = branchingAction Join Token.Join name

fork :: Parser Action
fork = branchingAction Fork Token.Fork name

alternative :: Parser Action
alternative = branchingAction Alternative Token.Alternative label

match :: Parser Action
match = branchingAction Match Token.Match label

portMap :: Parser PortMap
portMap = between Token.Brace $ port `sepBy` Token.Comma
  where
    side =
      (punctuation Token.LeftArrow $> LeftSide)
        <|> (punctuation Token.RightArrow $> RightSide)
    port = located $ (,,) <$> name <*> side <*> name
