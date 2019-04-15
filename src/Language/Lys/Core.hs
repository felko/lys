module Language.Lys.Core where

import Language.Lys.Types

-- | The core representation of processes
data Process
    = NilP
    | ParP Process Process
    | NewP String (Maybe Type) Process
    | OutputP Name Name Process
    | InputP Name String Process
    | ReplicateP Name String Process
    | InjectP Name String Name Process
    | MatchP Name [Branch Process]
    | CallP String [Name]
    | SourceP String String Process
    | ContractP String String Process Process
    deriving (Eq, Show)
