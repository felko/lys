-- | Core syntax.
module Language.Pion.Syntax.Core
  ( Type(..),
    Context,
    Sequent(..),
    module Language.Pion.Name,
    module Language.Pion.Syntax.Common,
    module Language.Pion.Type,
  )
where

import qualified Data.HashMap.Strict as HashMap
import Language.Pion.SourceSpan
import Language.Pion.Name
import Language.Pion.Syntax.Common
import Language.Pion.Pretty
import Language.Pion.Type

-- | Abstract syntax tree of types.
data Type
  = CompositeT ConnectiveType (HashMap.HashMap Name Type)
  | ModalityT ModalityType (Located Type)
  | VariableT Identifier
  deriving (Eq, Show)

instance Pretty Type where
  pretty = \case
    CompositeT connective types ->
      prettySyntaxNode
        "Composite"
        [ ("connective", pretty (show @Text connective)),
          ("types", prettyTypes)
        ]
      where
        prettyTypes =
          prettySyntaxList
            [ prettySyntaxField (pretty f) (pretty t)
              | (f, t) <- HashMap.toList types
            ]
    ModalityT modality type' ->
      prettySyntaxNode
        "Modality"
        [ ("modality", pretty (show @Text modality)),
          ("type", pretty type')
        ]
    VariableT identifier ->
      prettyLabelled "Variable" $ pretty identifier

-- | A typing context.
type Context = Branches Name Type

-- | Process types.
data Sequent = Sequent
  { sequentAntecedents :: Context,
    sequentSuccedents :: Context
  }
  deriving (Eq, Show)

instance Pretty Sequent where
  pretty Sequent {..} =
    prettySyntaxNode
      "Sequent"
      [ ("antecedents", pretty sequentAntecedents),
        ("succedents", pretty sequentSuccedents)
      ]

data Process
  = -- | (!D) derel u → x; P
    DerelL (Located Name) (Located Name) Process
  | -- | (?D) P; u ← derel x
    DerelR Process (Located Name) (Located Name)
  | -- | (?L) promote u → x; P
    PromoteL (Located Name) (Located Name) Process
  | -- | P; u ← promote x
    PromoteR Process (Located Name) (Located Name)
  | -- | weaken u; P
    WeakenL (Located Name) Process
  | -- | P; weaken u
    WeakenR Process (Located Name)
  | -- | contract u → (u1, u2); P
    ContractL (Located Name) [Located Name] Process
  | -- | P; (u1, u2) ← contract u
    ContractR Process [Located Name] (Located Name)
  | -- | x → y; P
    PipeL (Located Name) (Located Name) Process
  | -- | P; y ← x
    PipeR Process (Located Name) (Located Name)
  | -- | x → ~y; P
    SwapL (Located Name) (Located Name) Process
  | -- | P; y ← ~x
    SwapR Process (Located Name) (Located Name)
  | -- | run p { x ← a, y → b }
    Run (Located Identifier) PortMap
  | -- | fork z { x : P | y : Q }
    ForkL (Located Name) (Branches Name Process)
  | -- | join z { x : P | y : Q }
    ForkR (Located Name) (Branches Name Process)
  | -- | split abc { a → x, b → y, c → z }; P
    SplitL (Located Name) (HashMap.HashMap Name Name) Process
  | -- | P; split z {x → y}
    SplitR Process (Located Name) (HashMap.HashMap Name Name)
  | -- | match z { x : P | y : Q }
    MatchL (Located Name) (Branches Label Process)
  | -- | alt z { x : P | y : Q }
    MatchR (Located Name) (Branches Label Process)
  | -- | select abc.b → b; P
    SelectL (Located Name) (Located Label) (Located Name) Process
  | -- | P; x ← select l y
    SelectR Process (Located Name) (Located Label) (Located Name)
  deriving (Eq, Ord, Show)

-- instance Pretty Action where
--   pretty = \case
--     PreA name pat ->
--       prettySyntaxNode
--         "Pre"
--         [ ("name", pretty name),
--           ("pattern", pretty pat)
--         ]
--     PostA name expr ->
--       prettySyntaxNode
--         "Post"
--         [ ("name", pretty name),
--           ("expression", pretty expr)
--         ]
--     RunA ident ports ->
--       prettySyntaxNode
--         "Run"
--         [ ("ident", pretty ident),
--           ("ports", pretty ports)
--         ]
--     JoinA name procs ->
--       prettySyntaxNode
--         "Join"
--         [ ("name", pretty name),
--           ("processes", pretty procs)
--         ]
--     ForkA name procs ->
--       prettySyntaxNode
--         "Fork"
--         [ ("name", pretty name),
--           ("processes", pretty procs)
--         ]
--     AlternativeA name procs ->
--       prettySyntaxNode
--         "Alternative"
--         [ ("name", pretty name),
--           ("processes", pretty procs)
--         ]
--     MatchA name procs ->
--       prettySyntaxNode
--         "Match"
--         [ ("name", pretty name),
--           ("processes", pretty procs)
--         ]

-- data Process = Process
--   {procActions :: [Located Action]}
--   deriving (Eq, Ord, Show)

-- instance Pretty Process where
--   pretty Process {..} =
--     prettyLabelled "Process" $
--       prettySyntaxList (fmap pretty procActions)
