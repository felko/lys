-- | Pattern parser.
module Language.Pion.Parser
  ( -- * Types
    type',
    connT,
    expT,
    unitT,
    sequent,
    context,

    -- * Patterns
    pattern',
    prefixedPattern,
    varP,
    splitP,
    selectP,
    extractP,
    unwrapP,
    copyP,

    -- * Expressions
    literal,
    expression,
    varE,
    absE,
    appE,
    litE,
    tupleE,
    altE,
    matchE,
    letE,

    -- * Processes and actions
    process,
    action,
    inputA,
    outputA,
    runA,
    joinA,
    forkA,
    altA,
    matchA,

    -- * Top level declarations
    module',
    declaration,
    typeDeclaration,
    processDeclaration,
    functionDeclaration,
  )
where

import qualified Language.Pion.Lexer.Token as Token
import Language.Pion.Parser.Monad
import Language.Pion.SourceSpan
import Language.Pion.Syntax.Concrete
import qualified Text.Megaparsec as Mega

-- | Parse a type.
type' :: Parser (Located Type)
type' =
  located
    ( connT
        <|> unitT
        <|> expT
        <|> varT
        <?> "type"
    )

-- | Parse a composite type.
-- A composite type is described by a logical connective and then
-- either a list of types or a list of @field : Type@ pairs.
--
-- Examples:
--
--   * Ordered connective
--
-- > ⅋[Int, Char, String]
-- > ⨁[A, B, C, D]
--
--   * Labelled connective
--
-- > ⨂{ x : Int, y : Int }
-- > &{ left : A, right : B }
connT :: Parser Type
connT =
  ConnT
    <$> connectiveType
    <*> conjunction
      Token.Brace
      Token.Brack
      Token.Colon
      type'

-- | Parse an unit type.
-- There is a unit type for all connectives: @⨂@, @⅋@, @⨁@ and @&@ have unit
-- types @end@, @⊥@, @void@ and @⊤@ respectively.
--
-- A unit type is equivalent to an empty composite type of the same connective,
-- whether ordered or labelled, e.g. @⊤ = &[] = &{}@. All composite types and unit
-- types are reduced to labelled connectives when desugaring.
unitT :: Parser Type
unitT = UnitT <$> unitType

-- | Parse an exponential type.
-- Exponentials can be either @?@ or @!@, then followed by any type.
expT :: Parser Type
expT =
  ExpT
    <$> modalityType
    <*> type'

-- | Parse a type identifier.
-- No distinction is made between quantified type variables and a type name
-- that refers to a type alias.
varT :: Parser Type
varT = VarT <$> identifier

-- | Parse a logical context as a sequence of type annotations of the form
-- @name : Type@ separated by commas.
context :: Parser Context
context =
  fmap Branches $
    branch Token.Colon (located name) type'
      `sepBy` Token.Comma

-- | Parse a sequent, denoted by two contexts separated by a turnstile @⊢@.
sequent :: Parser (Located Sequent)
sequent = located do
  sequentAntecedents <- context
  punctuation Token.Turnstile
  sequentSuccedents <- context
  pure Sequent {..}

-- | Parse a pattern.
pattern' :: Parser (Located Pattern)
pattern' =
  prefixedPattern
    <|> located varP
    <?> "pattern"

-- | Parse a pattern that has a prefixing keyword.
prefixedPattern :: Parser (Located Pattern)
prefixedPattern =
  located
    ( splitP
        <|> selectP
        <|> extractP
        <|> unwrapP
        <|> copyP
        <?> "prefixed pattern"
    )

-- | Parse a variable pattern.
-- The variable name must be a valid identifier.
varP :: Parser Pattern
varP = VarP <$> name

-- | Parse a split pattern.
--
-- Example:
--
--  * Splitting on a tuple:
--
-- > split (x, y, z)
--
--  * Splitting on a record:
--
-- > split (x = a, y = b, z = c)
splitP :: Parser Pattern
splitP =
  keyword Token.Split
    >> SplitP <$> conjunction Token.Paren Token.Paren Token.Equal pattern'

-- | Parse a selector pattern.
--
-- Example:
--
-- > select succ n
selectP :: Parser Pattern
selectP =
  keyword Token.Select
    >> SelectP <$> located name <*> pattern'

-- | Parse an extract pattern.
--
-- Example:
--
-- > extract x
extractP :: Parser Pattern
extractP =
  keyword Token.Extract
    >> ExtractP <$> pattern'

-- | Parse an unwrap pattern.
--
-- Example:
--
-- > unwrap x
unwrapP :: Parser Pattern
unwrapP =
  keyword Token.Unwrap
    >> UnwrapP <$> pattern'

-- | Parse a copy pattern.
--
-- Example:
--
-- copy (x, y, z)
copyP :: Parser Pattern
copyP =
  keyword Token.Copy
    >> CopyP <$> between Token.Paren (pattern' `sepBy` Token.Comma)

-- | Parse a literal value.
literal :: Parser Literal
literal =
  (IntL <$> integerLiteral)
    <|> (FloatL <$> floatLiteral)
    <|> (CharL <$> charLiteral)
    <|> (StringL <$> stringLiteral)
    <?> "literal"

-- | Parse an expression.
expression :: Parser (Located Expression)
expression =
  matchE
    <|> letE
    <|> absE
    <|> appE
    <?> "expression"

-- | Parse an expression factor in an application.
-- A factor can be used in function applications.
factor :: Parser (Located Expression)
factor =
  litE
    <|> try tupleE
    <|> altE
    <|> varE
    <|> between Token.Paren expression
    <?> "expression"

-- | Parse a single variable.
varE :: Parser (Located Expression)
varE = located $ VarE <$> name

-- | Parse a lambda abstraction.
--
-- Example:
--
-- > λ x ⊸ x + x
absE :: Parser (Located Expression)
absE = located do
  punctuation Token.Lambda
  var <- located name
  punctuation Token.Lollipop
  body <- expression
  pure (AbsE var body)

-- | Parse a function application.
-- Both the function and the arguments have to be either a literal, a tuple, an
-- alternative, a variable, or a parenthesized expression.
--
-- Example:
--
-- > f x 8 {x : "abc", y : 5} (1 + 3)
appE :: Parser (Located Expression)
appE = do
  terms <- some factor
  let firstSpan = locSpan (head terms)
  pure $ foldl1' (foldApp firstSpan) terms
  where
    foldApp spanAcc f x =
      Located
        { locNode = AppE f x,
          locSpan = spanAcc <> locSpan x
        }

-- | Parse a literal value.
-- See 'Literal.literal'.
litE :: Parser (Located Expression)
litE = located $ LitE <$> literal

-- | Parse a record or a tuple.
-- A tuple is either a list of comma-separated expressions or fields,
-- enclosed in parenthesis.
--
-- Examples:
--
--   * Tuple
--
-- > (1, 2, 3)
--
--   * Record
--
-- > (x = 1, y = 2, z = 3)
tupleE :: Parser (Located Expression)
tupleE =
  located $
    TupleE
      <$> conjunction
        Token.Paren
        Token.Paren
        Token.Equal
        expression

-- | Parse an alternative.
-- An alternative is described by either a list of expressions, or by a list of
-- fields, in both cases enclosed by angle brackets (@⟨@ and @⟩@).
--
-- Examples:
--
--   * Ordered alternative:
--
-- > ⟨1, 2, 3⟩
--
--   * Labelled alternative:
--
-- > ⟨x = 1, y = 2, z = 3⟩
altE :: Parser (Located Expression)
altE =
  located $
    AltE
      <$> conjunction
        Token.Angle
        Token.Angle
        Token.Equal
        expression

-- | Parse a pattern match expression.
--
-- Examples:
--
-- > match n
-- >   { select zero: 0
-- >   | select succ n: n + 1
-- >   }
matchE :: Parser (Located Expression)
matchE =
  located $
    keyword Token.Match
      >> MatchE
        <$> expression
        <*> branches
          Token.Brace
          Token.Bar
          Token.Colon
          pattern'
          expression

-- | Parse let bindings.
-- A single let binding can omit the @let@ keyword when the binding pattern
-- has a prefix (see 'prefixedPattern').
-- Multiple variables can be bound simultaneously by enclosing the definitions
-- in braces, separated by semicolons.
--
-- Examples:
--
--   * Single let binding
--
-- > let x = 123 in x
-- > let select succ nMinusOne = n in nMinusOne
--
--   * Single let bindings with prefixed pattern
--
-- > split {quot, rem} = divMod 1 2 in rem
-- > select succ nMinusOne = n in nMinusOne
--
--   * Multiple let bindings
--
-- > let {
-- >   split (x, y) = f 10;
-- >   z = 2;
-- > } in x + y + z
letE :: Parser (Located Expression)
letE = located do
  bindings <- (Branches . pure <$> singleLetBinding) <|> aggregateLetBindings
  keyword Token.In
  body <- expression
  pure $ LetE bindings body

-- | Parse a let binding that starts with a given pattern parser.
letBindingWith :: Parser (Located Pattern) -> Parser (Branch Pattern Expression)
letBindingWith patternParser = do
  bound <- patternParser
  punctuation Token.Equal
  value <- expression
  pure (Branch bound value)

-- | Parse a single let binding, whether it is explicit (starts with @let@)
-- or implicit (where the pattern is prefixed by a disambiguating keyword).
singleLetBinding :: Parser (Branch Pattern Expression)
singleLetBinding =
  letBindingWith prefixedPattern
    <|> (keyword Token.Let *> letBindingWith pattern')

-- | Parse many let bindings enclosed in braces and separated by semicolons.
aggregateLetBindings :: Parser (Branches Pattern Expression)
aggregateLetBindings =
  keyword Token.Let
    *> ( Branches
           <$> between
             Token.Brace
             (letBindingWith pattern' `sepBy` Token.Semicolon)
       )

-- | Parse a process.
process :: Parser (Located Process)
process = located $ Process <$> action `sepBy` Token.Semicolon

-- | Parse an action.
action :: Parser (Located Action)
action =
  located $
    ( try runA
        <|> try inputA
        <|> try outputA
        <|> joinA
        <|> forkA
        <|> altA
        <|> matchA
        <?> "action"
    )

-- | Parse a pre-action.
-- A pre-action destructures an input with an irrefutable pattern.
--
-- Examples:
--
-- > x → select succ y
-- > x → split {a, b}
inputA :: Parser Action
inputA = do
  n <- located name
  punctuation Token.RightArrow
  p <- pattern'
  pure $ InputA n p

-- | Parse a post-action.
-- A post-action constructs an output from an expression
--
-- Examples:
--
-- > x ← select succ y
-- > x ← {a, b}
outputA :: Parser Action
outputA = do
  n <- located name
  punctuation Token.LeftArrow
  e <- expression
  pure $ OutputA n e

-- | Parse a process call.
-- The process ports can be remapped.
--
-- Example:
--
-- > run doSomething
-- > run divMod { a ← width, b ← height, q → line, r → column }
runA :: Parser Action
runA = do
  keyword Token.Run
  procName <- located identifier
  ports <- Mega.option [] portMap
  pure (RunA procName ports)

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
      Token.Colon
      (located caseParser)
      process

-- | Parse a process join.
--
-- Example:
--
-- > join z { x : P | y : Q }
joinA :: Parser Action
joinA = branchingAction JoinA Token.Join name

-- | Parse a process fork.
--
-- Example:
--
-- > fork z { x : P | y : Q }
forkA :: Parser Action
forkA = branchingAction ForkA Token.Fork name

-- | Parse process alternatives.
--
-- Example:
--
-- > alt z { x : P | y : Q }
altA :: Parser Action
altA = branchingAction AltA Token.Alt label

-- | Parse a matching process.
--
-- Contrary to match expressions (see 'Language.Pion.Parser.Expression.match'),
-- match processes cannot use patterns to destructure a value.
--
-- Example:
--
-- > match z { x : P | y : Q }
matchA :: Parser Action
matchA = branchingAction MatchA Token.Match label

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

declaration :: Parser (Located Declaration)
declaration =
  located $
    (TypeDecl <$> typeDeclaration)
      <|> (ProcDecl <$> processDeclaration)
      <|> (FuncDecl <$> functionDeclaration)

typeDeclaration :: Parser TypeDeclaration
typeDeclaration = do
  keyword Token.Type
  typeDeclName <- located identifier
  punctuation Token.Equal
  typeDeclType <- type'
  pure TypeDeclaration {..}

processDeclaration :: Parser ProcessDeclaration
processDeclaration = do
  keyword Token.Proc
  procDeclName <- located identifier
  punctuation Token.DoubleColon
  procDeclType <- sequent
  procDeclBody <- process
  pure ProcessDeclaration {..}

functionDeclaration :: Parser FunctionDeclaration
functionDeclaration = do
  keyword Token.Func
  funcDeclName <- located identifier
  punctuation Token.Colon
  funcDeclType <- type'
  funcDeclBody <- expression
  pure FunctionDeclaration {..}

module' :: Parser Module
module' = Module <$> many declaration
