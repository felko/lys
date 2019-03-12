module Language.Lys.Types where

import qualified Data.Map as Map
import qualified Data.Set as Set

type Fields = Map.Map String Type
type Params = [(String, Type)]
type Args   = [Name]
type Label  = String

data Type
    = IntT | FloatT | CharT | StringT
    | ProdT Fields
    | RecordT Type
    | VariantT Type
    | ExtendT Label Type Type
    | EmptyT
    | QuoteT Session
    | IdentT String
    | VarT String
    deriving (Eq, Ord, Show)

data Session
    = ReadS Name Session
    | WriteS Name
    | ParS Session Session
    | ProcS String Type Session
    | NilS
    | VarS String
    deriving (Eq, Ord, Show)

data Process
    = InputP  Name String Process
    | OutputP Name Name
    | NewP String Type Process
    | NewPI String Process
    | ParP Process Process
    | ProcP String Type Process
    | ProcPI String Process
    | CallP Process Name
    | VarP String
    | DropP Name
    | NilP
    deriving (Eq, Ord, Show)

data Name
    = FieldN Name String
    | LitN Literal
    | RecN [(String, Name)]
    | CaseN Name String
    | QuoteN Process
    | VarN String
    deriving (Eq, Ord, Show)

data Literal
    = IntL Integer
    | FloatL Float
    | CharL Char
    | StringL String
    deriving (Eq, Ord, Show)

recordToList :: Type -> ([(Label, Type)], Maybe String)
recordToList (VarT r)        = ([], Just r)
recordToList EmptyT          = ([], Nothing)
recordToList (ExtendT l t r) = let (ls, mv) = recordToList r
                                in ((l, t):ls, mv)
