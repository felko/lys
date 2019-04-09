module Language.Lys.TypeChecking.Match where
    
data Pattern a
    = NilPat
    | ParPat (Pattern a) (Pattern a)
    | OutputPat a a (Pattern a)
    | InputPat a a (Pattern a)
    | ReplicatePat a a (Pattern a)
    | InjectPat a String (Pattern a)
    | CasePat a [(String, Pattern a)]
    deriving (Eq, Show)
