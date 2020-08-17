-- | Syntax of processes
module Language.Pion.Syntax.Process
  ( Side (..),
    PortMap,
    Action (..),
    Process (..),
  )
where

import Language.Pion.Name
import Language.Pion.Pretty
import Language.Pion.SourceSpan
import Language.Pion.Syntax.Branch
import Language.Pion.Syntax.Expression (Expression)
import Language.Pion.Syntax.Pattern (Pattern)

data Side = LeftSide | RightSide
  deriving (Eq, Ord, Show)

type PortMap = [Located (Name, Side, Name)]

data Action
  = -- | p → x; P
    Pre (Located Name) (Located Pattern)
  | -- | P; x ← e
    Post (Located Name) (Located Expression)
  | -- | run p { x ← a, y → b }
    Run (Located Identifier) PortMap
  | -- | join z { x : P | y : Q }
    Join (Located Name) (Branches Name Process)
  | -- | fork z { x : P | y : Q }
    Fork (Located Name) (Branches Name Process)
  | -- | alt z { x : P | y : Q }
    Alternative (Located Name) (Branches Label Process)
  | -- | match z { x : P | y : Q }
    Match (Located Name) (Branches Label Process)
  deriving (Eq, Ord, Show)

instance Pretty Action where
  pretty = \case
    Pre name pat ->
      prettyASTNode
        "Pre"
        [ ("name", pretty name),
          ("pattern", pretty pat)
        ]
    Post name expr ->
      prettyASTNode
        "Post"
        [ ("name", pretty name),
          ("expression", pretty expr)
        ]
    Run ident ports ->
      let prettyPort Located {locNode = (p, s, q)} =
            case s of
              LeftSide -> pretty p <+> "←" <+> pretty q
              RightSide -> pretty p <+> "→" <+> pretty q
       in prettyASTNode
            "Run"
            [ ("ident", pretty ident),
              ( "ports",
                prettyLabelled "PortMap"
                  . prettyASTList
                  $ fmap prettyPort ports
              )
            ]
    Join name procs ->
      prettyASTNode
        "Join"
        [ ("name", pretty name),
          ("processes", pretty procs)
        ]
    Fork name procs ->
      prettyASTNode
        "Fork"
        [ ("name", pretty name),
          ("processes", pretty procs)
        ]
    Alternative name procs ->
      prettyASTNode
        "Alt"
        [ ("name", pretty name),
          ("processes", pretty procs)
        ]
    Match name procs ->
      prettyASTNode
        "Match"
        [ ("name", pretty name),
          ("processes", pretty procs)
        ]

data Process = Process
  {procActions :: [Located Action]}
  deriving (Eq, Ord, Show)

instance Pretty Process where
  pretty Process {..} =
    prettyLabelled "Process" $
      prettyASTList (fmap pretty procActions)
