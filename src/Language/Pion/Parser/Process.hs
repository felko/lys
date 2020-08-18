-- | Process parser.
module Language.Pion.Parser.Process
  ( process,
    action,
    preAction,
    postAction,
    run,
    join,
    fork,
    alternative,
    match,
  )
where

import qualified Language.Pion.Lexer.Token as Token
import Language.Pion.Name
import Language.Pion.Parser.Expression (expression)
import Language.Pion.Parser.Monad
import Language.Pion.Parser.Pattern (pattern')
import Language.Pion.SourceSpan
import Language.Pion.Syntax.Branch
import Language.Pion.Syntax.Process
import qualified Text.Megaparsec as Mega
import Prelude hiding (join)

-- | Parse a process.
process :: Parser (Located Process)
process = located $ Process <$> action `sepBy` Token.Semicolon

-- | Parse an action.
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

-- | Parse a pre-action.
-- A pre-action destructures an input with an irrefutable pattern.
--
-- Examples:
--
-- > x → select succ y
-- > x → split {a, b}
preAction :: Parser Action
preAction = do
  n <- located name
  punctuation Token.RightArrow
  p <- pattern'
  pure $ Pre n p

-- | Parse a post-action.
-- A post-action constructs an output from an expression
--
-- Examples:
--
-- > x ← select succ y
-- > x ← {a, b}
postAction :: Parser Action
postAction = do
  n <- located name
  punctuation Token.LeftArrow
  e <- expression
  pure $ Post n e

-- | Parse a process call.
-- The process ports can be remapped.
--
-- Example:
--
-- > run doSomething
-- > run divMod { a ← width, b ← height, q → line, r → column }
run :: Parser Action
run = do
  keyword Token.Run
  procName <- located identifier
  ports <- Mega.option [] portMap
  pure (Run procName ports)

-- | Parse a branching action from its constructor, prefixing keyword, and
-- branch label parser.
--
-- It parses the given keyword, followed by a name and then some branches,
-- enclosed in braces and separated by a vertical bar.
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

-- | Parse a process join.
--
-- Example:
--
-- > join z { x : P | y : Q }
join :: Parser Action
join = branchingAction Join Token.Join name

-- | Parse a process fork.
--
-- Example:
--
-- > fork z { x : P | y : Q }
fork :: Parser Action
fork = branchingAction Fork Token.Fork name

-- | Parse process alternatives.
--
-- Example:
--
-- > alternative z { x : P | y : Q }
alternative :: Parser Action
alternative = branchingAction Alternative Token.Alternative label

-- | Parse a matching process.
--
-- Contrary to match expressions (see 'Language.Pion.Parser.Expression.match'),
-- match processes cannot use patterns to destructure a value.
--
-- Example:
--
-- > match z { x : P | y : Q }
match :: Parser Action
match = branchingAction Match Token.Match label

-- | Parse a port map.
--
-- Port maps rename inputs and outputs when calling a process (see 'run' for an
-- exampel).
portMap :: Parser PortMap
portMap = between Token.Brace $ port `sepBy` Token.Comma
  where
    side =
      (punctuation Token.LeftArrow $> LeftSide)
        <|> (punctuation Token.RightArrow $> RightSide)
    port = located $ (,,) <$> name <*> side <*> name
